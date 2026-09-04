module UI.Registry
  ( Registry
  , build
  , rebuild
  , setBusy
  , setIdle
  , setSensitive
  , setViewMode
  , applyTableState
  ) where

import Control.Monad (forM, forM_, when)
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw

import Config (Config (..), TableFilters, TableSort, ViewMode (..), viewMode)
import Presentation.Row (RowSpec (..), ToolRows (..))
import Toolchain.Types (Progress, RowKey, SupportedTool)
import UI.ToolPanes (ToolPane (..), ToolPanes (..))
import UI.ToolPanes qualified as ToolPanes
import UI.View (RowCallbacks, View (..))
import UI.View.List qualified as ListView
import UI.View.Table qualified as TableView

data Registry = Registry
  { panes :: ToolPanes
  , views :: Map (ViewMode, SupportedTool) View
  , modeRef :: IORef ViewMode
  , planRef :: IORef (Map (ViewMode, SupportedTool) ToolRows)
  -- ^ Diff cache, keyed by renderer: the same plan must be rebuilt again for
  -- a renderer that has not drawn it yet.
  , busyRef :: IORef (Map RowKey Progress)
  , tables :: Map SupportedTool TableView.Table
  }

build
  :: Adw.ApplicationWindow
  -> ToolPanes
  -> Config
  -> TableView.TableCallbacks
  -> IO Registry
build window panes config tableCallbacks = do
  built <- forM panes.panes $ \pane -> do
    listView <- ListView.build window
    table <-
      TableView.build window config.tableSort config.tableFilters tableCallbacks
    ToolPanes.addView pane Simple listView.widget
    ToolPanes.addView pane Advanced table.view.widget
    pure
      ( [((Simple, pane.tool), listView), ((Advanced, pane.tool), table.view)]
      , (pane.tool, table)
      )
  let views = Map.fromList (concatMap fst (Vector.toList built))
      tables = Map.fromList (map snd (Vector.toList built))
  ToolPanes.setViewMode panes (viewMode config)
  Registry panes views
    <$> newIORef (viewMode config)
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> pure tables

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
  let liveKeys =
        Map.fromList
          [ (spec.key, ())
          | toolRows <- Map.elems plan
          , spec <- Vector.toList toolRows.rows
          ]
  modifyIORef' registry.busyRef (`Map.intersection` liveKeys)
  where
    currentEntries mode plan' reg =
      Map.fromList
        [ ((mode, pane.tool), Map.findWithDefault (ToolRows Vector.empty "") pane.tool plan')
        | pane <- Vector.toList reg.panes.panes
        ]

setViewMode :: Registry -> ViewMode -> IO ()
setViewMode registry mode = do
  current <- readIORef registry.modeRef
  when (current /= mode) $ do
    writeIORef registry.modeRef mode
    ToolPanes.setViewMode registry.panes mode
    busy <- readIORef registry.busyRef
    withVisibleViews registry $ \view ->
      forM_ (Map.toList busy) $ \(key, progress) -> view.setBusy key progress

applyTableState :: Registry -> TableSort -> TableFilters -> IO ()
applyTableState registry sort filters =
  forM_ (Map.elems registry.tables) $ \table -> table.applyState sort filters

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
