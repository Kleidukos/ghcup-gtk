module Development.Reload (loadAndWatchCSS) where

import Control.Exception (catch)
import Control.Monad (void, when)
import Data.GI.Base
import Data.IORef
import Data.Text qualified as Text
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

import Paths_ghcup_gtk qualified as Paths

loadAndWatchCSS :: Gtk.CssProvider -> IO ()
loadAndWatchCSS provider = do
  path <- resolveCssPath
  Gtk.cssProviderLoadFromPath provider path
  watchCSS provider path

resolveCssPath :: IO FilePath
resolveCssPath = do
  let localPath = "assets/style.css"
  exists <- doesFileExist localPath
  if exists then pure localPath else Paths.getDataFileName localPath

watchCSS :: Gtk.CssProvider -> FilePath -> IO ()
watchCSS provider path =
  watch `catch` \(err :: GError) -> do
    msg <- gerrorMessage err
    logDev ("CSS live-reload disabled: " <> Text.unpack msg)
  where
    watch = do
      file <- Gio.fileNewForPath path
      monitor <- Gio.fileMonitorFile file [Gio.FileMonitorFlagsWatchMoves] (Nothing @Gio.Cancellable)
      logDev ("watching " <> path)
      pendingReload <- newIORef Nothing
      on monitor #changed $ \_ _ eventType ->
        when (eventType `elem` reloadEvents) (scheduleReload pendingReload)
      -- Disown the monitor so it never gets GC'd.
      void (disownObject monitor)

    scheduleReload pendingReload = do
      readIORef pendingReload >>= mapM_ GLib.sourceRemove
      sourceId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 200 $ do
        writeIORef pendingReload Nothing
        Gtk.cssProviderLoadFromPath provider path
        logDev ("reloaded " <> path)
        pure False
      writeIORef pendingReload (Just sourceId)

    reloadEvents =
      [ Gio.FileMonitorEventChangesDoneHint
      , Gio.FileMonitorEventRenamed
      , Gio.FileMonitorEventMovedIn
      , Gio.FileMonitorEventCreated
      ]

logDev :: String -> IO ()
logDev msg = hPutStrLn stderr ("ghcup-gtk: " <> msg)
