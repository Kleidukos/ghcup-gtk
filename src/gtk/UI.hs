{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}

module UI (main) where

import Control.Monad (forM_, void)
import Data.GI.Base
import Data.IORef
import Data.Int
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (showVersion)
import Data.Word (Word32)
import Effectful
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
#ifdef DEVELOPMENT
import Development.Reload qualified as Reload
#endif
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Config qualified
import Effects.FileSystem (runFileSystemIO)
import Paths_ghcup_gtk qualified as Paths
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
      , On #startup loadCSS
      ]
  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)

{- FOURMOLU_DISABLE -}
loadCSS :: (MonadIO m) => m ()
loadCSS = do
  display <-
    Gdk.displayGetDefault
      >>= \case
        Nothing -> error "Could not find Display!"
        Just d -> pure d
  cssProvider <- Gtk.cssProviderNew
#ifdef DEVELOPMENT
  liftIO (Reload.loadAndWatchCSS cssProvider)
#else
  cssPath <- liftIO (Paths.getDataFileName "data/style.css")
  Gtk.cssProviderLoadFromPath cssProvider cssPath
#endif
  Gtk.styleContextAddProviderForDisplay
    display
    cssProvider
    (fromIntegral @Int32 @Word32 Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
{- FOURMOLU_ENABLE -}

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
  dirs <- GHCup.ghcupDirs
  (config, configWarning) <- runEff (runFileSystemIO Config.load)
  shell <- Shell.build app config
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
  on shell.window #closeRequest $ do
    (width, height) <- shell.window.getDefaultSize
    dispatch $
      Session.ConfigChanged $
        Config.SetWindowSize (fromIntegral width) (fromIntegral height)
    pure False

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
  Session.SwitchRenderer mode plan sort filters ->
    Registry.switchTo rt.registry (callbacks rt) mode plan sort filters
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
    Preferences.present shell.window model.config (dispatch . Session.ConfigChanged)
  app.addAction prefsAction

  shortcutsAction <- Gio.simpleActionNew "shortcuts" Nothing
  on shortcutsAction #activate $ \_ -> Shortcuts.present shell.window
  app.addAction shortcutsAction

  quitAction <- Gio.simpleActionNew "quit" Nothing
  on quitAction #activate $ const app.quit
  app.addAction quitAction

  aboutAction <- Gio.simpleActionNew "about" Nothing
  on aboutAction #activate $ \_ -> do
    about <-
      new
        Adw.AboutDialog
        [ #applicationName := "Haskell Toolchain Manager"
        , #version := Text.pack (showVersion Paths.version)
        , #developerName := "Hécate Moonlight"
        , #comments := "A GTK4 frontend for the ghcup toolchain manager"
        , #website := "https://www.haskell.org/ghcup/"
        , #licenseType := Gtk.LicenseBsd3
        ]
    about.present (Just shell.window)
  app.addAction aboutAction

  app.setAccelsForAction "app.preferences" ["<Control>comma"]
  app.setAccelsForAction "app.shortcuts" ["<Control>question"]
  app.setAccelsForAction "app.quit" ["<Control>q"]
