module UI.ToolPanes
  ( ToolPane (..)
  , ToolPanes (..)
  , build
  , displayName
  , onToolSelected
  , selectFirst
  , setChild
  ) where

import Control.Monad (forM, forM_, void)
import Data.GI.Base
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Toolchain.Types (SupportedTool (..), supportedTools)

displayName :: SupportedTool -> Text
displayName = \case
  GHC -> "Glasgow Haskell Compiler"
  Cabal -> "Cabal project manager"
  HLS -> "Haskell Language Server"
  Stack -> "Stack"

-- | Stable 'Gtk.Stack' page identifier. Deliberately not 'toolName':
-- that is user-facing copy and may be reworded; this may not.
pageName :: SupportedTool -> Text
pageName = \case
  GHC -> "ghc"
  Cabal -> "cabal"
  HLS -> "hls"
  Stack -> "stack"

-- | One tool's widgets: its sidebar entry and the bin that holds the
-- active renderer. Only one renderer exists at a time; switching modes
-- replaces the bin's child ('UI.Registry.switchTo').
data ToolPane = ToolPane
  { tool :: SupportedTool
  , sidebarRow :: Adw.ActionRow
  , bin :: Adw.Bin
  }

data ToolPanes = ToolPanes
  { sidebar :: Gtk.ListBox
  , pages :: Gtk.Stack
  , panes :: Vector ToolPane
  -- ^ In 'supportedTools' order, which is also the sidebar row order.
  }

build :: IO ToolPanes
build = do
  sidebar <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeBrowse]
  sidebar.addCssClass "navigation-sidebar"
  pages <- new Gtk.Stack []

  panes <- forM supportedTools $ \tool -> do
    sidebarRow <- new Adw.ActionRow [#title := displayName tool]
    sidebar.append sidebarRow
    bin <- new Adw.Bin []
    pages.addNamed bin (Just (pageName tool))
    pure ToolPane {tool, sidebarRow, bin}

  pure ToolPanes {sidebar, pages, panes}

-- | Mount a renderer in a pane, dropping (and thereby destroying) the
-- previous one.
setChild :: ToolPane -> Gtk.Widget -> IO ()
setChild pane widget = pane.bin.setChild (Just widget)

selectFirst :: ToolPanes -> IO ()
selectFirst toolPanes =
  forM_ (Vector.take 1 toolPanes.panes) $ \pane ->
    toolPanes.sidebar.selectRow (Just pane.sidebarRow)

-- | Selecting a tool switches the outer 'pages' stack to that tool's pane.
onToolSelected :: ToolPanes -> (SupportedTool -> IO ()) -> IO ()
onToolSelected toolPanes handler =
  void $ on toolPanes.sidebar #rowSelected $ \case
    Nothing -> pure ()
    Just selected -> do
      idx <- selected.getIndex
      forM_ (toolPanes.panes Vector.!? fromIntegral idx) $ \pane -> do
        toolPanes.pages.setVisibleChildName (pageName pane.tool)
        handler pane.tool
