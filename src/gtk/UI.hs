{-# LANGUAGE ImplicitParams #-}

module UI (main) where

import Control.Monad (forM_, void)
import Data.GI.Base
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import Effectful (runEff)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Config qualified
import Effects.FileSystem (runFileSystemIO)
import Session qualified
import Toolchain.GHCup qualified as GHCup
import Toolchain.Path (applyFix, checkPath)
import Toolchain.Types
import UI.Dialog qualified as Dialog
import UI.HeaderBar
import UI.PathBanner qualified as PathBanner
import UI.PreferencesWindow qualified as PreferencesWindow
import UI.Row qualified as Row
import UI.RowRegistry qualified as RowRegistry
import UI.ToolList qualified as ToolList
import Worker qualified

main :: IO ()
main = do
  app <-
    new
      Adw.Application
      [ #applicationId := "org.haskell.GhcupGtk"
      , On #activate (activate ?self)
      ]
  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)

data Shell = Shell
  { window :: Adw.ApplicationWindow
  , toastOverlay :: Adw.ToastOverlay
  , stack :: Gtk.Stack
  , staleBanner :: Adw.Banner
  , panes :: ToolList.ToolPanes
  , pathBanner :: PathBanner.Handle
  , retryButton :: Gtk.Button
  }

data Runtime = Runtime
  { app :: Adw.Application
  , shell :: Shell
  , registry :: RowRegistry.Registry
  , worker :: Worker.Handle
  , dirs :: GhcupDirs
  , dispatch :: Session.Event -> IO ()
  }

activate :: Adw.Application -> IO ()
activate app = do
  shell <- buildShell app
  registry <- RowRegistry.new shell.window shell.panes
  dirs <- GHCup.ghcupDirs
  (config, configWarning) <- runEff (runFileSystemIO Config.load)
  modelRef <- newIORef (Session.initialModel dirs config)
  worker <- Worker.new

  let runtime = Runtime {app, shell, registry, worker, dirs, dispatch}
      dispatch event = do
        model <- readIORef modelRef
        let (model', effects) = Session.step event model
        writeIORef modelRef model'
        mapM_ (interpretEffect runtime) effects

      notify msg =
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
          dispatch (Session.WorkerMsg msg)
          pure False

  Worker.start worker notify

  forM_ configWarning $ \warning -> showToast shell warning 5

  installActions app shell modelRef dispatch
  on shell.retryButton #clicked $ dispatch Session.RetryClicked

  Worker.enqueue worker RefreshListings
  runPathCheck runtime
  shell.window.present

-- | Widget construction only: no model, no worker, no callbacks.
buildShell :: Adw.Application -> IO Shell
buildShell app = do
  window <-
    new
      Adw.ApplicationWindow
      [ #application := app
      , #defaultWidth := 760
      , #defaultHeight := 560
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

  panes <- ToolList.newToolPanes

  stack <- new Gtk.Stack []
  stack.addNamed loadingSpinner (Just "loading")
  stack.addNamed offlinePage (Just "offline")
  stack.addNamed panes.pages (Just "list")
  stack.setVisibleChildName "loading"

  staleBanner <-
    new
      Adw.Banner
      [ #title := "Version data may be outdated — network unreachable"
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

  ToolList.onToolSelected panes $ \tool -> do
    set contentPage [#title := ToolList.displayName tool]
    splitView.setShowContent True
  ToolList.selectFirst panes

  toastOverlay <- new Adw.ToastOverlay [#child := splitView]
  set window [#content := toastOverlay]

  pure Shell {window, toastOverlay, stack, staleBanner, panes, pathBanner, retryButton}

navPage :: Text -> Text -> Adw.HeaderBar -> Gtk.Widget -> IO Adw.NavigationPage
navPage title tag header content = do
  view <- new Adw.ToolbarView [#content := content]
  view.addTopBar header
  new Adw.NavigationPage [#child := view, #title := title, #tag := tag]

interpretEffect :: Runtime -> Session.Effect -> IO ()
interpretEffect rt = \case
  Session.Enqueue job -> Worker.enqueue rt.worker job
  Session.Hold -> rt.app.hold
  Session.Release -> rt.app.release
  Session.SetSensitive b -> ToolList.setSensitive rt.shell.panes b
  Session.SwitchPage phase -> rt.shell.stack.setVisibleChildName (pageOf phase)
  Session.RevealStaleBanner b -> rt.shell.staleBanner.setRevealed b
  Session.Toast title -> showToast rt.shell title 3
  Session.ErrorToast err -> showErrorToast rt.shell err
  Session.SetBusy key progress -> RowRegistry.setBusy rt.registry key progress
  Session.SetIdle key -> RowRegistry.setIdle rt.registry key
  Session.Rerender plan -> RowRegistry.rebuild rt.registry (callbacks rt) plan
  Session.SaveConfig newConfig ->
    runEff (runFileSystemIO (Config.save newConfig)) >>= \case
      Left e -> hPutStrLn stderr ("ghcup-gtk: could not save config: " <> Text.unpack e)
      Right () -> pure ()
  Session.CheckPath -> runPathCheck rt
  Session.ApplyPathFix changes -> do
    result <- runEff (runFileSystemIO (applyFix changes))
    rt.dispatch (Session.PathFixDone result)
  Session.SetPathBanner spec ->
    PathBanner.render rt.shell.pathBanner (rt.dispatch Session.PathFixConfirmed) spec
  Session.SetViewMode _ -> pure ()
  Session.SetTableState _ _ -> pure ()
  where
    pageOf :: Session.Phase -> Text
    pageOf = \case
      Session.Loading -> "loading"
      Session.Offline -> "offline"
      Session.Ready -> "list"

callbacks :: Runtime -> Row.RowCallbacks
callbacks rt = Row.RowCallbacks {onSubmit = rt.dispatch . Session.Submitted . Mutate}

runPathCheck :: Runtime -> IO ()
runPathCheck rt = do
  status <- runEff (runFileSystemIO (checkPath rt.dirs))
  rt.dispatch (Session.PathChecked status)

showToast :: Shell -> Text -> Word32 -> IO ()
showToast shell title timeout = do
  toast <- new Adw.Toast [#title := title, #timeout := timeout]
  shell.toastOverlay.addToast toast

showErrorToast :: Shell -> OpError -> IO ()
showErrorToast shell err = do
  toast <-
    new Adw.Toast [#title := err.title, #timeout := 8, #buttonLabel := "Details"]
  on toast #buttonClicked $ Dialog.info shell.window err.title err.details
  shell.toastOverlay.addToast toast

installActions
  :: Adw.Application
  -> Shell
  -> IORef Session.Model
  -> (Session.Event -> IO ())
  -> IO ()
installActions app shell modelRef dispatch = do
  prefsAction <- Gio.simpleActionNew "preferences" Nothing
  on prefsAction #activate $ \_ -> do
    model <- readIORef modelRef
    PreferencesWindow.present shell.window model.config (dispatch . Session.ConfigChanged)
  app.addAction prefsAction

  aboutAction <- Gio.simpleActionNew "about" Nothing
  on aboutAction #activate $ \_ -> do
    about <-
      new
        Adw.AboutDialog
        [ #applicationName := "ghcup-gtk"
        , #version := "0.1.0.0"
        , #developerName := "Hécate Moonlight"
        , #comments := "A GTK4 frontend for the ghcup toolchain manager"
        , #website := "https://www.haskell.org/ghcup/"
        , #licenseType := Gtk.LicenseGpl30Only
        ]
    about.present (Just shell.window)
  app.addAction aboutAction
