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
import UI.PathBanner qualified as PathBanner
import UI.Preferences qualified as Preferences
import UI.Registry qualified as Registry
import UI.Shell (Shell (..))
import UI.Shell qualified as Shell
import UI.Shortcuts qualified as Shortcuts
import UI.View (RowCallbacks (..))
import UI.View.Table qualified as TableView
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

data Runtime = Runtime
  { app :: Adw.Application
  , shell :: Shell
  , registry :: Registry.Registry
  , worker :: Worker.Handle
  , dirs :: GhcupDirs
  , dispatch :: Session.Event -> IO ()
  }

activate :: Adw.Application -> IO ()
activate app = do
  shell <- Shell.build app
  dirs <- GHCup.ghcupDirs
  (config, configWarning) <- runEff (runFileSystemIO Config.load)
  modelRef <- newIORef (Session.initialModel dirs config)
  worker <- Worker.new

  dispatchRef <- newIORef (\_ -> pure ())
  registry <-
    Registry.build shell.window shell.panes config $
      tableCallbacks (\event -> readIORef dispatchRef >>= ($ event))

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

  writeIORef dispatchRef dispatch

  Worker.start worker notify

  forM_ configWarning $ \warning -> showToast shell warning 5

  installActions app shell modelRef dispatch
  on shell.retryButton #clicked $ dispatch Session.RetryClicked

  Worker.enqueue worker RefreshListings
  runPathCheck runtime
  shell.window.present

interpretEffect :: Runtime -> Session.Effect -> IO ()
interpretEffect rt = \case
  Session.Enqueue job -> Worker.enqueue rt.worker job
  Session.Hold -> rt.app.hold
  Session.Release -> rt.app.release
  Session.SetSensitive b -> Registry.setSensitive rt.registry b
  Session.SwitchPage phase -> rt.shell.stack.setVisibleChildName (pageOf phase)
  Session.RevealStaleBanner b -> rt.shell.staleBanner.setRevealed b
  Session.Toast title -> showToast rt.shell title 3
  Session.ErrorToast err -> showErrorToast rt.shell err
  Session.SetBusy key progress -> Registry.setBusy rt.registry key progress
  Session.SetIdle key -> Registry.setIdle rt.registry key
  Session.Rerender plan -> Registry.rebuild rt.registry (callbacks rt) plan
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
  Session.SwitchRenderer mode plan -> do
    Registry.setViewMode rt.registry mode
    Registry.rebuild rt.registry (callbacks rt) plan
  Session.SetTableState sort filters -> Registry.applyTableState rt.registry sort filters
  where
    pageOf :: Session.Phase -> Text
    pageOf = \case
      Session.Loading -> "loading"
      Session.Offline -> "offline"
      Session.Ready -> "list"

callbacks :: Runtime -> RowCallbacks
callbacks rt = RowCallbacks {onSubmit = rt.dispatch . Session.Submitted . Mutate}

tableCallbacks :: (Session.Event -> IO ()) -> TableView.TableCallbacks
tableCallbacks dispatch =
  TableView.TableCallbacks
    { onSortChanged = dispatch . Session.ConfigChanged . Config.SetTableSort
    , onFiltersChanged = dispatch . Session.ConfigChanged . Config.SetTableFilters
    }

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
