module UI.Shell
  ( Shell (..)
  , build
  ) where

import Data.GI.Base
import Data.Text (Text)
import Data.Text.Display
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Config (Config (..))
import Presentation.Row ()
import UI.HeaderBar
import UI.PathBanner qualified as PathBanner
import UI.ToolPanes qualified as ToolPanes

data Shell = Shell
  { window :: Adw.ApplicationWindow
  , toastOverlay :: Adw.ToastOverlay
  , stack :: Gtk.Stack
  , staleBanner :: Adw.Banner
  , panes :: ToolPanes.ToolPanes
  , pathBanner :: PathBanner.Handle
  , retryButton :: Gtk.Button
  }

-- | Widget construction only: no model, no worker, no callbacks.
build :: Adw.Application -> Config -> IO Shell
build app config = do
  window <-
    new
      Adw.ApplicationWindow
      [ #application := app
      , #defaultWidth := fromIntegral config.windowWidth
      , #defaultHeight := fromIntegral config.windowHeight
      , #title := "Haskell Toolchain Manager"
      ]

  loadingSpinner <-
    new
      Gtk.Spinner
      [ #spinning := True
      , #halign := Gtk.AlignCenter
      , #valign := Gtk.AlignCenter
      , #widthRequest := 48
      , #heightRequest := 48
      ]
  offlinePage <-
    new
      Adw.StatusPage
      [ #title := "No Network Connection"
      , #description := "Version data could not be fetched."
      ]
  retryButton <- new Gtk.Button [#label := "Retry", #halign := Gtk.AlignCenter]
  retryButton.addCssClass "suggested-action"
  offlinePage.setChild (Just retryButton)

  panes <- ToolPanes.build

  stack <- new Gtk.Stack []
  stack.addNamed loadingSpinner (Just "loading")
  stack.addNamed offlinePage (Just "offline")
  stack.addNamed panes.pages (Just "list")
  stack.setVisibleChildName "loading"

  staleBanner <-
    new
      Adw.Banner
      [ #title := "Version data may be outdated – network unreachable"
      , #revealed := False
      ]
  contentBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  pathBanner <- PathBanner.build window
  contentBox.append pathBanner.widget
  contentBox.append staleBanner
  contentBox.append stack

  contentHeader <- new Adw.HeaderBar []
  contentPage <- navPage "Tools" "content" contentHeader =<< Gtk.toWidget contentBox

  sidebarHeader <- genHeaderbar
  sidebarScrolled <-
    new Gtk.ScrolledWindow [#child := panes.sidebar, #vexpand := True]
  sidebarPage <- navPage "Tools" "sidebar" sidebarHeader =<< Gtk.toWidget sidebarScrolled

  splitView <-
    new
      Adw.NavigationSplitView
      [ #sidebar := sidebarPage
      , #content := contentPage
      ]

  breakpoint <-
    Adw.breakpointNew =<< Adw.breakpointConditionParse "max-width: 560sp"
  collapsed <- toGValue True
  breakpoint.addSetter splitView "collapsed" (Just collapsed)
  window.addBreakpoint breakpoint

  ToolPanes.onToolSelected panes $ \tool -> do
    set contentPage [#title := display tool]
    splitView.setShowContent True

  toastOverlay <- new Adw.ToastOverlay [#child := splitView]
  set window [#content := toastOverlay]

  pure Shell {window, toastOverlay, stack, staleBanner, panes, pathBanner, retryButton}

navPage :: Text -> Text -> Adw.HeaderBar -> Gtk.Widget -> IO Adw.NavigationPage
navPage title tag header content = do
  view <- new Adw.ToolbarView [#content := content]
  view.addTopBar header
  new Adw.NavigationPage [#child := view, #title := title, #tag := tag]
