module UI.Registry
  ( Registry
  , ViewState (..)
  , build
  , invalidate
  , reconcile
  ) where

import Control.Monad (forM, forM_, when)
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import GHCup.Types (Tool)

import Presentation.Filter (ActiveFilters, Channel, channelsFor, reachableChannels, seedFilters)
import Presentation.Row (ToolRows (..))
import Session (ChannelsEditability)
import Toolchain.Channels (BaseChannel)
import Toolchain.Types (sortTools)
import UI.ToolPanes (ToolPane (..), ToolPanes (..))
import UI.ToolPanes qualified as ToolPanes
import UI.View (RowCallbacks, View (..))
import UI.View.List qualified as ListView

-- | The slice of the session model the widget tree must reflect.
data ViewState = ViewState
  { channels :: Set Channel
  , base :: BaseChannel
  , editable :: ChannelsEditability
  , sensitive :: Bool
  , plan :: Map Tool ToolRows
  }

-- | One live renderer per tool. 'reconcile' rebuilds them when the pane
-- set, the url-source base, or the offered channels change. A channel that
-- no pane offers must not cause a rebuild: it loses scroll position for
-- nothing.
data Registry = Registry
  { panes :: ToolPanes
  , rowCallbacks :: RowCallbacks
  -- ^ Retained so 'reconcile' can build fresh renderers.
  , onBaseChanged :: BaseChannel -> IO ()
  , renderersRef :: IORef (Map Tool View)
  , appliedRef :: IORef (Maybe ViewState)
  -- ^ The last state applied to the widgets. 'Nothing' until the first
  -- 'reconcile', and treated as 'Nothing' after a rebuild so that the full
  -- state is replayed onto the fresh widgets.
  }

build
  :: ToolPanes
  -> RowCallbacks
  -> (BaseChannel -> IO ())
  -> IO Registry
build panes rowCallbacks onBaseChanged =
  Registry panes rowCallbacks onBaseChanged
    <$> newIORef Map.empty
    <*> newIORef Nothing

-- | Build one renderer per tool and mount it in its pane. Each renderer
-- keeps the filter selections of its predecessor. Without a predecessor,
-- and for a newly offered channel, channels start visible.
buildRenderers :: Registry -> Set Channel -> ViewState -> Map Tool ActiveFilters -> IO (Map Tool View)
buildRenderers registry previousChannels state carried = do
  currentPanes <- readIORef registry.panes.panesRef
  built <- forM currentPanes $ \pane -> do
    let offered = channelsFor state.channels pane.tool
        initial = case Map.lookup pane.tool carried of
          Nothing -> seedFilters [] offered mempty
          Just filters -> seedFilters (channelsFor previousChannels pane.tool) offered filters
    view <-
      ListView.build
        offered
        state.base
        state.editable
        initial
        registry.rowCallbacks
        registry.onBaseChanged
    ToolPanes.setChild pane view.widget
    pure (pane.tool, view)
  pure (Map.fromList (Vector.toList built))

invalidate :: Registry -> IO ()
invalidate registry = writeIORef registry.appliedRef Nothing

reconcile :: Registry -> ViewState -> IO ()
reconcile registry state = do
  applied <- readIORef registry.appliedRef
  let tools = Vector.fromList (sortTools (Map.keys state.plan))
  panesChanged <- ToolPanes.sync registry.panes tools
  let channelsChanged = case applied of
        Nothing -> True
        Just prev ->
          prev.base /= state.base
            || ( prev.channels /= state.channels
                   && reachableChannels prev.channels tools /= reachableChannels state.channels tools
               )
      needRebuild = panesChanged || channelsChanged
  renderers <-
    if needRebuild
      then do
        carried <- readIORef registry.renderersRef >>= traverse (.getFilters)
        let previousChannels = maybe Set.empty (.channels) applied
        renderers <- buildRenderers registry previousChannels state carried
        writeIORef registry.renderersRef renderers
        pure renderers
      else readIORef registry.renderersRef

  let prev = if needRebuild then Nothing else applied

  when (fmap (.sensitive) prev /= Just state.sensitive) $ do
    set registry.panes.sidebar [#sensitive := state.sensitive]
    forM_ (Map.elems renderers) $ \view ->
      view.setSensitive state.sensitive

  let prevPlan = maybe Map.empty (.plan) prev
  currentPanes <- readIORef registry.panes.panesRef
  forM_ currentPanes $ \pane -> do
    let toolRows = Map.findWithDefault (ToolRows Vector.empty "" []) pane.tool state.plan
    set pane.sidebarRow [#subtitle := toolRows.subtitle]
    when (Map.lookup pane.tool prevPlan /= Just toolRows) $
      forM_ (Map.lookup pane.tool renderers) $ \view ->
        view.setRows toolRows

  writeIORef registry.appliedRef (Just state)
