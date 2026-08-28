module UI.Registry
  ( Registry
  , applyListState
  , applyTableState
  , build
  , rebuild
  , setSensitive
  , switchTo
  ) where

import Control.Monad (forM, forM_, when)
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw

import Config (Config (..), Filters, TableSort, ViewMode (..), viewMode)
import Presentation.Row (ToolRows (..))
import Toolchain.Types (SupportedTool)
import UI.ToolPanes (ToolPane (..), ToolPanes (..))
import UI.ToolPanes qualified as ToolPanes
import UI.View (RowCallbacks, View (..))
import UI.View.List qualified as ListView
import UI.View.Table qualified as TableView

-- | The renderer a pane holds in the current mode.
data Renderer
  = TableRenderer TableView.Table
  | ListRenderer ListView.ListView

viewOf :: Renderer -> View
viewOf = \case
  TableRenderer table -> table.view
  ListRenderer list -> list.view

-- | One live renderer per tool. Only the active mode's renderers exist,
-- becase 'switchTo' destroys them and builds the other mode's.
data Registry = Registry
  { panes :: ToolPanes
  , window :: Adw.ApplicationWindow
  -- ^ Retained so 'switchTo' can build fresh renderers.
  , tableCallbacks :: TableView.TableCallbacks
  -- ^ Likewise.
  , listCallbacks :: ListView.ListCallbacks
  -- ^ Likewise.
  , modeRef :: IORef ViewMode
  , renderersRef :: IORef (Map SupportedTool Renderer)
  , planRef :: IORef (Map SupportedTool ToolRows)
  , sensitiveRef :: IORef Bool
  }

build
  :: Adw.ApplicationWindow
  -> ToolPanes
  -> Config
  -> TableView.TableCallbacks
  -> ListView.ListCallbacks
  -> IO Registry
build window panes config tableCallbacks listCallbacks = do
  renderers <- buildRenderers window panes tableCallbacks listCallbacks config
  Registry panes window tableCallbacks listCallbacks
    <$> newIORef (viewMode config)
    <*> newIORef renderers
    <*> newIORef Map.empty
    <*> newIORef True

-- | Build one renderer per tool and mount each in its pane
buildRenderers
  :: Adw.ApplicationWindow
  -> ToolPanes
  -> TableView.TableCallbacks
  -> ListView.ListCallbacks
  -> Config
  -> IO (Map SupportedTool Renderer)
buildRenderers window panes tableCallbacks listCallbacks config = do
  built <- forM panes.panes $ \pane -> do
    renderer <- case viewMode config of
      Simple ->
        ListRenderer <$> ListView.build window config.listFilters listCallbacks
      Advanced ->
        TableRenderer
          <$> TableView.build window config.tableSort config.tableFilters tableCallbacks
    ToolPanes.setChild pane (viewOf renderer).widget
    pure (pane.tool, renderer)
  pure (Map.fromList (Vector.toList built))

rebuild :: Registry -> RowCallbacks -> Map SupportedTool ToolRows -> IO ()
rebuild registry callbacks plan = do
  prev <- readIORef registry.planRef
  renderers <- readIORef registry.renderersRef
  forM_ registry.panes.panes $ \pane -> do
    let toolRows = Map.findWithDefault (ToolRows Vector.empty "") pane.tool plan
    set pane.sidebarRow [#subtitle := toolRows.subtitle]
    when (Map.lookup pane.tool prev /= Just toolRows) $
      forM_ (Map.lookup pane.tool renderers) $ \renderer ->
        (viewOf renderer).setRows callbacks toolRows
  writeIORef registry.planRef plan

switchTo
  :: Registry
  -> RowCallbacks
  -> Map SupportedTool ToolRows
  -> Config
  -> IO ()
switchTo registry callbacks plan config = do
  current <- readIORef registry.modeRef
  when (current /= viewMode config) $ do
    renderers <-
      buildRenderers
        registry.window
        registry.panes
        registry.tableCallbacks
        registry.listCallbacks
        config
    writeIORef registry.modeRef (viewMode config)
    writeIORef registry.renderersRef renderers
    writeIORef registry.planRef Map.empty
    sensitive <- readIORef registry.sensitiveRef
    forM_ (Map.elems renderers) $ \renderer -> (viewOf renderer).setSensitive sensitive
    rebuild registry callbacks plan

applyTableState :: Registry -> TableSort -> Filters -> IO ()
applyTableState registry sort filters = do
  renderers <- readIORef registry.renderersRef
  forM_ (Map.elems renderers) $ \case
    TableRenderer table -> table.applyState sort filters
    ListRenderer _ -> pure ()

applyListState :: Registry -> Filters -> IO ()
applyListState registry filters = do
  renderers <- readIORef registry.renderersRef
  forM_ (Map.elems renderers) $ \case
    ListRenderer list -> list.applyFilters filters
    TableRenderer _ -> pure ()

setSensitive :: Registry -> Bool -> IO ()
setSensitive registry b = do
  writeIORef registry.sensitiveRef b
  set registry.panes.sidebar [#sensitive := b]
  renderers <- readIORef registry.renderersRef
  forM_ (Map.elems renderers) $ \renderer -> (viewOf renderer).setSensitive b
