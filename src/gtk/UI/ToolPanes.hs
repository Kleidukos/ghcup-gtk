module UI.ToolPanes
  ( ToolPane (..)
  , ToolPanes (..)
  , build
  , onToolSelected
  , sync
  , setChild
  ) where

import Control.Monad (forM, forM_, void)
import Data.GI.Base
import Data.IORef
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHCup.Types (Tool)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.Row ()
import Toolchain.Types (isCoreTool, toolText)

data ToolPane = ToolPane
  { tool :: Tool
  , sidebarRow :: Adw.ActionRow
  , bin :: Adw.Bin
  }

data ToolPanes = ToolPanes
  { sidebar :: Gtk.Box
  , coreHeading :: Gtk.Label
  , coreList :: Gtk.ListBox
  , thirdPartyHeading :: Gtk.Label
  , thirdPartyList :: Gtk.ListBox
  , pages :: Gtk.Stack
  , panesRef :: IORef (Vector ToolPane)
  }

-- | An empty sidebar and stack; 'sync' populates them once a row plan
-- exists. The Loading page hides the emptiness until then.
build :: IO ToolPanes
build = do
  coreList <- sidebarList
  coreHeading <-
    new
      Gtk.Label
      [ #label := "Core Tools"
      , #xalign := 0
      , #visible := False
      , #marginStart := 12
      , #marginEnd := 12
      , #marginTop := 12
      , #marginBottom := 6
      , #cssClasses := ["caption-heading", "dim-label"]
      ]
  thirdPartyList <- sidebarList
  thirdPartyHeading <-
    new
      Gtk.Label
      [ #label := "Third-party Tools"
      , #xalign := 0
      , #visible := False
      , #marginStart := 12
      , #marginEnd := 12
      , #marginTop := 12
      , #marginBottom := 6
      , #cssClasses := ["caption-heading", "dim-label"]
      ]
  sidebar <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  sidebar.append coreHeading
  sidebar.append coreList
  sidebar.append thirdPartyHeading
  sidebar.append thirdPartyList
  pages <- new Gtk.Stack []
  panesRef <- newIORef Vector.empty
  pure
    ToolPanes
      { sidebar
      , coreHeading
      , coreList
      , thirdPartyHeading
      , thirdPartyList
      , pages
      , panesRef
      }
  where
    -- Single, not Browse: Browse forbids the programmatic unselect that
    -- keeps the two lists mutually exclusive.
    sidebarList = do
      list <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle]
      list.addCssClass "navigation-sidebar"
      pure list

-- | The list a tool's row lives in: default-channel tools on top,
-- third-party channel tools below.
listOf :: ToolPanes -> Tool -> Gtk.ListBox
listOf toolPanes tool
  | isCoreTool tool = toolPanes.coreList
  | otherwise = toolPanes.thirdPartyList

-- | Panes split between the two lists, in list order.
partitionPanes :: Vector ToolPane -> (Vector ToolPane, Vector ToolPane)
partitionPanes = Vector.partition (\pane -> isCoreTool pane.tool)

-- | Reconcile the panes with the ordered tool list. Returns 'True' when
-- the pane set changed (callers must then rebuild renderers).
sync :: ToolPanes -> Vector Tool -> IO Bool
sync toolPanes tools = do
  current <- readIORef toolPanes.panesRef
  if fmap (.tool) current == tools
    then pure False
    else do
      selected <- selectedTool toolPanes current
      forM_ current $ \pane -> do
        (listOf toolPanes pane.tool).remove pane.sidebarRow
        widget <- Gtk.toWidget pane.bin
        toolPanes.pages.remove widget
      panes <- forM tools $ \tool -> do
        sidebarRow <- new Adw.ActionRow [#title := display tool]
        (listOf toolPanes tool).append sidebarRow
        bin <- new Adw.Bin []
        toolPanes.pages.addNamed bin (Just (toolText tool))
        pure ToolPane {tool, sidebarRow, bin}
      writeIORef toolPanes.panesRef panes
      let hasThirdParty = Vector.any (\pane -> not (isCoreTool pane.tool)) panes
      set toolPanes.coreHeading [#visible := True]
      set toolPanes.thirdPartyHeading [#visible := hasThirdParty]
      let restored = selected >>= \tool -> Vector.find (\pane -> pane.tool == tool) panes
      case restored of
        Just pane -> selectPane pane
        Nothing -> forM_ (panes Vector.!? 0) selectPane
      pure True
  where
    selectPane pane =
      (listOf toolPanes pane.tool).selectRow (Just pane.sidebarRow)

-- | The tool of the currently selected sidebar row, if any.
selectedTool :: ToolPanes -> Vector ToolPane -> IO (Maybe Tool)
selectedTool toolPanes panes = do
  let (corePanes, thirdPartyPanes) = partitionPanes panes
  selectedIn toolPanes.coreList corePanes >>= \case
    Just tool -> pure (Just tool)
    Nothing -> selectedIn toolPanes.thirdPartyList thirdPartyPanes
  where
    selectedIn list subset =
      list.getSelectedRow >>= \case
        Nothing -> pure Nothing
        Just row -> do
          idx <- row.getIndex
          pure (fmap (.tool) (subset Vector.!? fromIntegral idx))

setChild :: ToolPane -> Gtk.Widget -> IO ()
setChild pane widget = pane.bin.setChild (Just widget)

onToolSelected :: ToolPanes -> (Tool -> IO ()) -> IO ()
onToolSelected toolPanes handler = do
  connect toolPanes.coreList toolPanes.thirdPartyList fst
  connect toolPanes.thirdPartyList toolPanes.coreList snd
  where
    connect list other side =
      void $ on list #rowSelected $ \case
        Nothing -> pure ()
        Just selected -> do
          other.unselectAll
          idx <- selected.getIndex
          panes <- readIORef toolPanes.panesRef
          let subset = side (partitionPanes panes)
          forM_ (subset Vector.!? fromIntegral idx) $ \pane -> do
            toolPanes.pages.setVisibleChildName (toolText pane.tool)
            handler pane.tool
