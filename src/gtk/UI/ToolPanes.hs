module UI.ToolPanes
  ( ToolPane (..)
  , ToolPanes (..)
  , addView
  , build
  , displayName
  , onToolSelected
  , selectFirst
  , setViewMode
  ) where

import Control.Monad (forM, forM_, void)
import Data.GI.Base
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Config (ViewMode (..))
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

-- | Stack page identifier of a renderer inside a pane.
viewName :: ViewMode -> Text
viewName = \case
  Simple -> "simple"
  Advanced -> "table"

-- | One tool's widgets: its sidebar entry and the stack that holds one page
-- per renderer.
data ToolPane = ToolPane
  { tool :: SupportedTool
  , sidebarRow :: Adw.ActionRow
  , stack :: Gtk.Stack
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
    stack <- new Gtk.Stack []
    pages.addNamed stack (Just (pageName tool))
    pure ToolPane {tool, sidebarRow, stack}

  pure ToolPanes {sidebar, pages, panes}

-- | Add a renderer's widget to a pane. Called once per renderer at startup.
-- 'Gtk.stackAddNamed' returns the 'Gtk.StackPage' it created, hence the 'void':
-- unlike the existing call sites this one is a whole function body, so
-- @-Wno-unused-do-bind@ does not cover it and the types would not match.
addView :: ToolPane -> ViewMode -> Gtk.Widget -> IO ()
addView pane mode widget = void $ pane.stack.addNamed widget (Just (viewName mode))

setViewMode :: ToolPanes -> ViewMode -> IO ()
setViewMode toolPanes mode =
  forM_ toolPanes.panes $ \pane -> pane.stack.setVisibleChildName (viewName mode)

selectFirst :: ToolPanes -> IO ()
selectFirst toolPanes =
  forM_ (Vector.take 1 toolPanes.panes) $ \pane ->
    toolPanes.sidebar.selectRow (Just pane.sidebarRow)

-- | Two stacks are in play and only one of them moves here: selecting a tool
-- switches the outer 'pages' stack to that tool's pane (as it always has), and
-- must leave the pane's inner renderer stack alone — only 'setViewMode' decides
-- which renderer a pane shows.
onToolSelected :: ToolPanes -> (SupportedTool -> IO ()) -> IO ()
onToolSelected toolPanes handler =
  void $ on toolPanes.sidebar #rowSelected $ \case
    Nothing -> pure ()
    Just selected -> do
      idx <- selected.getIndex
      forM_ (toolPanes.panes Vector.!? fromIntegral idx) $ \pane -> do
        toolPanes.pages.setVisibleChildName (pageName pane.tool)
        handler pane.tool
