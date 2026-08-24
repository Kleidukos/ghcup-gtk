module UI.Registry
  ( Registry
  , build
  , rebuild
  , setBusy
  , setIdle
  , setSensitive
  ) where

import Control.Monad (forM, forM_, when)
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw

import Config (Config, ViewMode (..))
import Presentation.Row (ToolRows (..))
import Toolchain.Types (Progress, RowKey, SupportedTool)
import UI.ToolPanes (ToolPane (..), ToolPanes (..))
import UI.ToolPanes qualified as ToolPanes
import UI.View (RowCallbacks, View (..))
import UI.View.List qualified as ListView

data Registry = Registry
  { panes :: ToolPanes
  , views :: Map (ViewMode, SupportedTool) View
  , modeRef :: IORef ViewMode
  , planRef :: IORef (Map (ViewMode, SupportedTool) ToolRows)
  -- ^ Diff cache, keyed by renderer: the same plan must be rebuilt again for
  -- a renderer that has not drawn it yet.
  , busyRef :: IORef (Map RowKey Progress)
  }

build :: Adw.ApplicationWindow -> ToolPanes -> Config -> IO Registry
build window panes _config = do
  viewList <- forM panes.panes $ \pane -> do
    listView <- ListView.build window
    ToolPanes.addView pane Simple listView.widget
    pure ((Simple, pane.tool), listView)
  let views = Map.fromList (Vector.toList viewList)
  -- Only the simple renderer exists in this task, so pin the panes to it
  -- regardless of the config: Task 9 adds the table page and switches on
  -- `viewMode config`. Without this pin, a config.kdl that already says
  -- advanced-interface #true would ask for a stack child that is not there.
  ToolPanes.setViewMode panes Simple
  Registry panes views
    <$> newIORef Simple
    <*> newIORef Map.empty
    <*> newIORef Map.empty

rebuild :: Registry -> RowCallbacks -> Map SupportedTool ToolRows -> IO ()
rebuild registry callbacks plan = do
  mode <- readIORef registry.modeRef
  prev <- readIORef registry.planRef
  busy <- readIORef registry.busyRef
  forM_ registry.panes.panes $ \pane -> do
    let toolRows = Map.findWithDefault (ToolRows Vector.empty "") pane.tool plan
        cacheKey = (mode, pane.tool)
    set pane.sidebarRow [#subtitle := toolRows.subtitle]
    when (Map.lookup cacheKey prev /= Just toolRows) $
      forM_ (Map.lookup cacheKey registry.views) $ \view -> do
        view.setRows callbacks toolRows
        forM_ (Map.toList busy) $ \(key, progress) -> view.setBusy key progress
  writeIORef registry.planRef (Map.union (currentEntries mode plan registry) prev)
  where
    currentEntries mode plan' reg =
      Map.fromList
        [ ((mode, pane.tool), Map.findWithDefault (ToolRows Vector.empty "") pane.tool plan')
        | pane <- Vector.toList reg.panes.panes
        ]

setBusy :: Registry -> RowKey -> Progress -> IO ()
setBusy registry key progress = do
  modifyIORef' registry.busyRef (Map.insert key progress)
  withVisibleViews registry $ \view -> view.setBusy key progress

setIdle :: Registry -> RowKey -> IO ()
setIdle registry key = do
  modifyIORef' registry.busyRef (Map.delete key)
  withVisibleViews registry $ \view -> view.setIdle key

setSensitive :: Registry -> Bool -> IO ()
setSensitive registry b = do
  set registry.panes.sidebar [#sensitive := b]
  withAllViews registry $ \view -> view.setSensitive b

withVisibleViews :: Registry -> (View -> IO ()) -> IO ()
withVisibleViews registry act = do
  mode <- readIORef registry.modeRef
  forM_ registry.panes.panes $ \pane ->
    forM_ (Map.lookup (mode, pane.tool) registry.views) act

withAllViews :: Registry -> (View -> IO ()) -> IO ()
withAllViews registry act = forM_ (Map.elems registry.views) act
