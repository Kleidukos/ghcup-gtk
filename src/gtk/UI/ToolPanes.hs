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
import Data.Text (Text)
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
  { sidebar :: Gtk.ListBox
  , pages :: Gtk.Stack
  , panesRef :: IORef (Vector ToolPane)
  }

-- | An empty sidebar and stack; 'sync' populates them once a row plan
-- exists. The Loading page hides the emptiness until then.
build :: IO ToolPanes
build = do
  sidebar <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeBrowse]
  sidebar.addCssClass "navigation-sidebar"
  pages <- new Gtk.Stack []
  panesRef <- newIORef Vector.empty
  Gtk.listBoxSetHeaderFunc sidebar $ Just $ \row mbefore -> do
    panes <- readIORef panesRef
    let sectionOf r = do
          idx <- r.getIndex
          pure (fmap (isCoreTool . (.tool)) (panes Vector.!? fromIntegral idx))
    this <- sectionOf row
    prev <- maybe (pure Nothing) sectionOf mbefore
    heading <- case this of
      Just core | this /= prev -> Just <$> sidebarHeading (headingLabel core)
      _ -> pure Nothing
    row.setHeader heading
  pure ToolPanes {sidebar, pages, panesRef}
  where
    headingLabel :: Bool -> Text
    headingLabel core = if core then "Core Tools" else "Third-party Tools"
    sidebarHeading label = do
      widget <-
        new
          Gtk.Label
          [ #label := label
          , #xalign := 0
          , #cssClasses := ["sidebar-heading", "caption-heading", "dim-label"]
          ]
      Gtk.toWidget widget

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
        toolPanes.sidebar.remove pane.sidebarRow
        widget <- Gtk.toWidget pane.bin
        toolPanes.pages.remove widget
      panes <- forM tools $ \tool -> do
        sidebarRow <- new Adw.ActionRow [#title := display tool]
        toolPanes.sidebar.append sidebarRow
        bin <- new Adw.Bin []
        toolPanes.pages.addNamed bin (Just (toolText tool))
        pure ToolPane {tool, sidebarRow, bin}
      writeIORef toolPanes.panesRef panes
      -- Rows were appended before panesRef held them, so the header func
      -- ran against the old vector; rerun it against the new one.
      toolPanes.sidebar.invalidateHeaders
      let restored = selected >>= \tool -> Vector.find (\pane -> pane.tool == tool) panes
      case restored of
        Just pane -> selectPane pane
        Nothing -> forM_ (panes Vector.!? 0) selectPane
      pure True
  where
    selectPane pane =
      toolPanes.sidebar.selectRow (Just pane.sidebarRow)

-- | The tool of the currently selected sidebar row, if any.
selectedTool :: ToolPanes -> Vector ToolPane -> IO (Maybe Tool)
selectedTool toolPanes panes =
  toolPanes.sidebar.getSelectedRow >>= \case
    Nothing -> pure Nothing
    Just row -> do
      idx <- row.getIndex
      pure (fmap (.tool) (panes Vector.!? fromIntegral idx))

setChild :: ToolPane -> Gtk.Widget -> IO ()
setChild pane widget = pane.bin.setChild (Just widget)

onToolSelected :: ToolPanes -> (Tool -> IO ()) -> IO ()
onToolSelected toolPanes handler =
  void $ on toolPanes.sidebar #rowSelected $ \case
    Nothing -> pure ()
    Just selected -> do
      idx <- selected.getIndex
      panes <- readIORef toolPanes.panesRef
      forM_ (panes Vector.!? fromIntegral idx) $ \pane -> do
        toolPanes.pages.setVisibleChildName (toolText pane.tool)
        handler pane.tool
