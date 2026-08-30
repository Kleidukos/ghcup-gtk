{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}

module UI (startUI) where

import Control.Monad (forM_, void, when)
import Data.Function ((&))
import Data.GI.Base
import Data.IORef
import Data.Int
import Data.Map.Strict qualified as Map
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
import System.Directory (doesDirectoryExist)
import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)

import CLI qualified
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

startUI :: IO ()
startUI = do
  options <- CLI.getOptions
  app <-
    new
      Adw.Application
      [ #applicationId := "org.haskell.GhcupGtk"
      , On #activate (activate options.forcedView ?self)
      , On #startup loadCSS
      , On #startup loadIcons
      ]
  progName <- getProgName
  void (app.run $ Just $ progName : options.gtkArgs)

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

-- | Make the bundled icons (data/icons) resolvable by name.
loadIcons :: (MonadIO m) => m ()
loadIcons = do
  display <-
    Gdk.displayGetDefault
      >>= \case
        Nothing -> error "Could not find Display!"
        Just d -> pure d
  theme <- Gtk.iconThemeGetForDisplay display
  localExists <- liftIO (doesDirectoryExist "data/icons")
  path <-
    if localExists
      then pure "data/icons"
      else liftIO (Paths.getDataFileName "data/icons")
  theme.addSearchPath path

data Runtime = Runtime
  { app :: Adw.Application
  , shell :: Shell
  , registry :: Registry.Registry
  , worker :: Worker.Handle
  , dirs :: GhcupDirs
  , modelRef :: IORef Session.Model
  , dispatch :: Session.Event -> IO ()
  }

activate :: Maybe Config.ViewMode -> Adw.Application -> IO ()
activate forcedView app = do
  dirs <- GHCup.ghcupDirs
  (loadedConfig, configWarning) <- runEff (runFileSystemIO Config.load)
  let config = case forcedView of
        Nothing -> loadedConfig
        Just mode -> loadedConfig {Config.viewMode = mode}
  shell <- Shell.build app config
  modelRef <- newIORef (Session.initialModel dirs config)
  worker <- Worker.new

  dispatchRef <- newIORef (\_ -> pure ())
  let dispatchLater event = readIORef dispatchRef >>= ($ event)
  registry <-
    Registry.build
      shell.panes
      (rowCallbacks dispatchLater)
      (tableCallbacks dispatchLater)

  let runtime = Runtime {app, shell, registry, worker, dirs, modelRef, dispatch}
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
  Session.Confirm confirmation job ->
    Dialog.confirm rt.shell.window confirmation $ \confirmed ->
      when confirmed $ rt.dispatch (Session.Submitted job)
  Session.Reconcile -> reconcile rt
  Session.Toast title -> showToast rt.shell title 3
  Session.ErrorToast err -> showErrorToast rt.shell err
  Session.SaveConfig newConfig ->
    runEff (runFileSystemIO (Config.save newConfig)) >>= \case
      Left e -> hPutStrLn stderr ("ghcup-gtk: could not save config: " <> Text.unpack e)
      Right () -> pure ()
  Session.CheckPath -> runPathCheck rt
  Session.ApplyPathFix changes -> do
    result <- runEff (runFileSystemIO (applyFix changes))
    rt.dispatch (Session.PathFixDone result)

reconcile :: Runtime -> IO ()
reconcile rt = do
  model <- readIORef rt.modelRef
  Registry.reconcile rt.registry (viewState model)
  rt.shell.stack.setVisibleChildName (pageOf model.phase)
  rt.shell.staleBanner.setRevealed (model.freshness == Stale)
  PathBanner.render
    rt.shell.pathBanner
    (rt.dispatch Session.PathFixConfirmed)
    (Session.bannerFor model)
  where
    pageOf :: Session.Phase -> Text
    pageOf = \case
      Session.Loading -> "loading"
      Session.Offline -> "offline"
      Session.Ready -> "list"

viewState :: Session.Model -> Registry.ViewState
viewState model =
  Registry.ViewState
    { config = model.config
    , sensitive = Map.null model.inFlight
    , plan = Session.rowPlan model
    }

rowCallbacks :: (Session.Event -> IO ()) -> RowCallbacks
rowCallbacks dispatch =
  RowCallbacks
    { onSubmit = \op -> Mutate op & Session.Submitted & dispatch
    , onConfirm = dispatch . Session.ConfirmRequested
    }

tableCallbacks :: (Session.Event -> IO ()) -> TableView.TableCallbacks
tableCallbacks dispatch =
  TableView.TableCallbacks
    { onSortChanged = \sort -> Config.SetTableSort sort & Session.ConfigChanged & dispatch
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
        , #licenseType := Gtk.LicenseGpl30Only
        ]
    about.present (Just shell.window)
  app.addAction aboutAction

  app.setAccelsForAction "app.preferences" ["<Control>comma"]
  app.setAccelsForAction "app.shortcuts" ["<Control>question"]
  app.setAccelsForAction "app.quit" ["<Control>q"]
