module UI.Registry
  ( Registry
  , ViewState (..)
  , build
  , reconcile
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import GHCup.Types (Tool)

import Config (Config (..), ViewMode (..))
import Presentation.Row (ToolRows (..))
import Toolchain.Types (sortTools)
import UI.ToolPanes (ToolPane (..), ToolPanes (..))
import UI.ToolPanes qualified as ToolPanes
import UI.View (RowCallbacks, View (..))
import UI.View.List qualified as ListView
import UI.View.Table qualified as TableView

-- | The slice of the session model the widget tree must reflect.
data ViewState = ViewState
  { config :: Config
  , sensitive :: Bool
  , plan :: Map Tool ToolRows
  }

-- | One live renderer per tool. Only the active mode's renderers exist:
-- 'reconcile' destroys them and builds the other mode's when the view
-- mode changes.
data Registry = Registry
  { panes :: ToolPanes
  , rowCallbacks :: RowCallbacks
  -- ^ Retained so 'reconcile' can build fresh renderers.
  , tableCallbacks :: TableView.TableCallbacks
  -- ^ Likewise.
  , renderersRef :: IORef (Map Tool View)
  , appliedRef :: IORef (Maybe ViewState)
  -- ^ The last state applied to the widgets; 'Nothing' until the first
  -- 'reconcile', and treated as 'Nothing' again after a renderer rebuild
  -- so everything is replayed onto the fresh widgets.
  }

build
  :: ToolPanes
  -> RowCallbacks
  -> TableView.TableCallbacks
  -> IO Registry
build panes rowCallbacks tableCallbacks =
  Registry panes rowCallbacks tableCallbacks
    <$> newIORef Map.empty
    <*> newIORef Nothing

-- | Build one renderer per tool and mount each in its pane
buildRenderers :: Registry -> Config -> IO (Map Tool View)
buildRenderers registry config = do
  currentPanes <- readIORef registry.panes.panesRef
  built <- forM currentPanes $ \pane -> do
    view <- case config.viewMode of
      Simple ->
        ListView.build pane.tool registry.rowCallbacks
      Advanced ->
        TableView.build pane.tool config registry.rowCallbacks registry.tableCallbacks
    ToolPanes.setChild pane view.widget
    pure (pane.tool, view)
  pure (Map.fromList (Vector.toList built))

reconcile :: Registry -> ViewState -> IO ()
reconcile registry new = do
  applied <- readIORef registry.appliedRef
  let tools = Vector.fromList (sortTools (Map.keys new.plan))
  panesChanged <- ToolPanes.sync registry.panes tools
  let modeChanged = fmap (.config.viewMode) applied /= Just new.config.viewMode
      needRebuild = panesChanged || modeChanged
  renderers <-
    if needRebuild
      then do
        renderers <- buildRenderers registry new.config
        writeIORef registry.renderersRef renderers
        pure renderers
      else readIORef registry.renderersRef

  let prev = if needRebuild then Nothing else applied

  when (fmap (.sensitive) prev /= Just new.sensitive) $ do
    set registry.panes.sidebar [#sensitive := new.sensitive]
    forM_ (Map.elems renderers) $ \view ->
      view.setSensitive new.sensitive

  unless needRebuild $
    when (fmap (.config) prev /= Just new.config) $
      forM_ (Map.elems renderers) $ \view ->
        view.applyConfig new.config

  let prevPlan = maybe Map.empty (.plan) prev
  currentPanes <- readIORef registry.panes.panesRef
  forM_ currentPanes $ \pane -> do
    let toolRows = Map.findWithDefault (ToolRows Vector.empty "" []) pane.tool new.plan
    set pane.sidebarRow [#subtitle := toolRows.subtitle]
    when (Map.lookup pane.tool prevPlan /= Just toolRows) $
      forM_ (Map.lookup pane.tool renderers) $ \view ->
        view.setRows toolRows

  writeIORef registry.appliedRef (Just new)
