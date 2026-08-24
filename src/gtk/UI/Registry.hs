module UI.Registry
  ( Registry
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

import Config (Config (..), TableFilters, TableSort, ViewMode (..), viewMode)
import Presentation.Row (ToolRows (..))
import Toolchain.Types (SupportedTool)
import UI.ToolPanes (ToolPane (..), ToolPanes (..))
import UI.ToolPanes qualified as ToolPanes
import UI.View (RowCallbacks, View (..))
import UI.View.List qualified as ListView
import UI.View.Table qualified as TableView

-- | One live renderer per tool. Only the active mode's views exist;
-- 'switchTo' destroys them and builds the other mode's.
data Registry = Registry
  { panes :: ToolPanes
  , window :: Adw.ApplicationWindow
  -- ^ Retained so 'switchTo' can build fresh renderers.
  , tableCallbacks :: TableView.TableCallbacks
  -- ^ Likewise.
  , modeRef :: IORef ViewMode
  , viewsRef :: IORef (Map SupportedTool View)
  , tablesRef :: IORef (Map SupportedTool TableView.Table)
  , planRef :: IORef (Map SupportedTool ToolRows)
  , sensitiveRef :: IORef Bool

  }

build
  :: Adw.ApplicationWindow
  -> ToolPanes
  -> Config
  -> TableView.TableCallbacks
  -> IO Registry
build window panes config tableCallbacks = do
  let mode = viewMode config
  (views, tables) <-
    buildViews window panes tableCallbacks mode config.tableSort config.tableFilters
  Registry panes window tableCallbacks
    <$> newIORef mode
    <*> newIORef views
    <*> newIORef tables
    <*> newIORef Map.empty
    <*> newIORef True

-- | Build one renderer per tool and mount each in its pane, dropping
-- whatever the pane held before.
buildViews
  :: Adw.ApplicationWindow
  -> ToolPanes
  -> TableView.TableCallbacks
  -> ViewMode
  -> TableSort
  -> TableFilters
  -> IO (Map SupportedTool View, Map SupportedTool TableView.Table)
buildViews window panes tableCallbacks mode sort filters = do
  built <- forM panes.panes $ \pane -> do
    (view, mtable) <- case mode of
      Simple -> do
        view <- ListView.build window
        pure (view, Nothing)
      Advanced -> do
        table <- TableView.build window sort filters tableCallbacks
        pure (table.view, Just table)
    ToolPanes.setChild pane view.widget
    pure (pane.tool, view, mtable)
  pure
    ( Map.fromList [(tool, view) | (tool, view, _) <- Vector.toList built]
    , Map.fromList [(tool, table) | (tool, _, Just table) <- Vector.toList built]
    )

rebuild :: Registry -> RowCallbacks -> Map SupportedTool ToolRows -> IO ()
rebuild registry callbacks plan = do
  prev <- readIORef registry.planRef
  views <- readIORef registry.viewsRef
  forM_ registry.panes.panes $ \pane -> do
    let toolRows = Map.findWithDefault (ToolRows Vector.empty "") pane.tool plan
    set pane.sidebarRow [#subtitle := toolRows.subtitle]
    when (Map.lookup pane.tool prev /= Just toolRows) $
      forM_ (Map.lookup pane.tool views) $ \view ->
        view.setRows callbacks toolRows
  writeIORef registry.planRef plan

-- | Interpret 'Session.SwitchRenderer': tear down the old renderers, build
-- the new mode's, and draw the carried plan into them. The plan carries
-- progress stamps, so a switch mid-mutation shows its spinners immediately.
switchTo
  :: Registry
  -> RowCallbacks
  -> ViewMode
  -> Map SupportedTool ToolRows
  -> TableSort
  -> TableFilters
  -> IO ()
switchTo registry callbacks mode plan sort filters = do
  current <- readIORef registry.modeRef
  when (current /= mode) $ do
    (views, tables) <-
      buildViews registry.window registry.panes registry.tableCallbacks mode sort filters
    writeIORef registry.modeRef mode
    writeIORef registry.viewsRef views
    writeIORef registry.tablesRef tables
    writeIORef registry.planRef Map.empty
    sensitive <- readIORef registry.sensitiveRef
    forM_ (Map.elems views) $ \view -> view.setSensitive sensitive
    rebuild registry callbacks plan

applyTableState :: Registry -> TableSort -> TableFilters -> IO ()
applyTableState registry sort filters = do
  tables <- readIORef registry.tablesRef
  forM_ (Map.elems tables) $ \table -> table.applyState sort filters

setSensitive :: Registry -> Bool -> IO ()
setSensitive registry b = do
  writeIORef registry.sensitiveRef b
  set registry.panes.sidebar [#sensitive := b]
  views <- readIORef registry.viewsRef
  forM_ (Map.elems views) $ \view -> view.setSensitive b
