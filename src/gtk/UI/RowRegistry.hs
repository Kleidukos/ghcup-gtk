module UI.RowRegistry
  ( Registry
  , new
  , rebuild
  , setBusy
  , setIdle
  ) where

import Control.Monad (forM, forM_)
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw

import Presentation.Row (RowSpec (..), ToolRows (..))
import Toolchain.Types (Progress, RowKey, SupportedTool)
import UI.Row (RowCallbacks, RowHandle (..))
import UI.Row qualified as Row
import UI.ToolList (ToolPane (..), ToolPanes (..))

data Registry = Registry
  { window :: Adw.ApplicationWindow
  , panes :: ToolPanes
  , handlesRef :: IORef (Map RowKey RowHandle)
  , busyRef :: IORef (Map RowKey Progress)
  , planRef :: IORef (Map SupportedTool ToolRows)
  }

new :: Adw.ApplicationWindow -> ToolPanes -> IO Registry
new window panes =
  Registry window panes
    <$> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty

rebuild :: Registry -> RowCallbacks -> Map SupportedTool ToolRows -> IO ()
rebuild registry callbacks plan = do
  prevPlan <- readIORef registry.planRef
  prevHandles <- readIORef registry.handlesRef
  handles <- forM registry.panes.panes $ \pane -> do
    let toolRows = Map.findWithDefault (ToolRows Vector.empty "") pane.tool plan
    if Map.lookup pane.tool prevPlan == Just toolRows
      then pure (keepHandles prevHandles toolRows)
      else do
        pane.list.removeAll
        set pane.sidebarRow [#subtitle := toolRows.subtitle]
        toolHandles <- forM toolRows.rows $ \spec -> do
          handle <- Row.build registry.window spec callbacks
          pane.list.append handle.row
          pure (spec.key, handle)

        case Vector.uncons (Vector.mapMaybe ((.defaultCheck) . snd) toolHandles) of
          Just (anchor, rest) -> forM_ rest $ \check -> check.setGroup (Just anchor)
          Nothing -> pure ()

        busy <- readIORef registry.busyRef
        forM_ toolHandles $ \(key, handle) ->
          forM_ (Map.lookup key busy) $ \progress -> handle.setBusy progress
        pure toolHandles
  writeIORef registry.planRef plan
  let handleMap = Map.fromList (foldMap Vector.toList handles)
  writeIORef registry.handlesRef handleMap
  modifyIORef' registry.busyRef (`Map.intersection` handleMap)

keepHandles :: Map RowKey RowHandle -> ToolRows -> Vector (RowKey, RowHandle)
keepHandles prevHandles toolRows =
  Vector.mapMaybe
    (\spec -> (,) spec.key <$> Map.lookup spec.key prevHandles)
    toolRows.rows

setBusy :: Registry -> RowKey -> Progress -> IO ()
setBusy registry key progress = do
  modifyIORef' registry.busyRef (Map.insert key progress)
  handles <- readIORef registry.handlesRef
  forM_ (Map.lookup key handles) $ \handle -> handle.setBusy progress

setIdle :: Registry -> RowKey -> IO ()
setIdle registry key = do
  modifyIORef' registry.busyRef (Map.delete key)
  handles <- readIORef registry.handlesRef
  forM_ (Map.lookup key handles) $ \handle -> handle.setIdle
