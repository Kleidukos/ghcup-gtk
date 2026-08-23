module UI.ToolList
  ( ToolPane (..)
  , ToolPanes (..)
  , displayName
  , newToolPanes
  , onToolSelected
  , selectFirst
  , setSensitive
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

-- | One tool's widgets: its sidebar entry and its version list.
data ToolPane = ToolPane
  { tool :: SupportedTool
  , sidebarRow :: Adw.ActionRow
  , list :: Gtk.ListBox
  }

data ToolPanes = ToolPanes
  { sidebar :: Gtk.ListBox
  , pages :: Gtk.Stack
  , panes :: Vector ToolPane
  -- ^ In 'supportedTools' order, which is also the sidebar row order.
  }

newToolPanes :: IO ToolPanes
newToolPanes = do
  sidebar <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeBrowse]
  sidebar.addCssClass "navigation-sidebar"
  pages <- new Gtk.Stack []

  panes <- forM supportedTools $ \tool -> do
    sidebarRow <- new Adw.ActionRow [#title := displayName tool]
    sidebar.append sidebarRow

    list <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
    list.addCssClass "boxed-list"
    clamp <-
      new
        Adw.Clamp
        [ #child := list
        , #maximumSize := 600
        , #tighteningThreshold := 400
        , #marginTop := 24
        , #marginBottom := 24
        , #marginStart := 12
        , #marginEnd := 12
        ]
    scrolled <-
      new
        Gtk.ScrolledWindow
        [ #child := clamp
        , #vexpand := True
        , #hscrollbarPolicy := Gtk.PolicyTypeNever
        ]
    pages.addNamed scrolled (Just (pageName tool))
    pure ToolPane {tool, sidebarRow, list}

  pure ToolPanes {sidebar, pages, panes}

selectFirst :: ToolPanes -> IO ()
selectFirst toolPanes =
  forM_ (Vector.take 1 toolPanes.panes) $ \pane ->
    toolPanes.sidebar.selectRow (Just pane.sidebarRow)

onToolSelected :: ToolPanes -> (SupportedTool -> IO ()) -> IO ()
onToolSelected toolPanes handler =
  void $ on toolPanes.sidebar #rowSelected $ \case
    Nothing -> pure ()
    Just selected -> do
      idx <- selected.getIndex
      forM_ (toolPanes.panes Vector.!? fromIntegral idx) $ \pane -> do
        toolPanes.pages.setVisibleChildName (pageName pane.tool)
        handler pane.tool

setSensitive :: ToolPanes -> Bool -> IO ()
setSensitive toolPanes b = do
  set toolPanes.sidebar [#sensitive := b]
  forM_ toolPanes.panes $ \pane -> set pane.list [#sensitive := b]
