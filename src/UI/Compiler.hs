module UI.Compiler where

import Data.GI.Base
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import UI.Install

getGHCVersions :: Adw.ToastOverlay -> Adw.ApplicationWindow -> IO [Adw.ActionRow]
getGHCVersions toastOverlay app = traverse (toActionRow toastOverlay app "GHC") compilerList
  where
    compilerList =
      [ "9.4.3 (latest)"
      , "9.2.5"
      , "9.0.2"
      , "8.10.7"
      ]

getCabalVersions :: Adw.ToastOverlay -> Adw.ApplicationWindow -> IO [Adw.ActionRow]
getCabalVersions toastOverlay app = traverse (toActionRow toastOverlay app "Cabal") cabalVersions
  where
    cabalVersions =
      [ "3.8.1.0 (latest)"
      , "3.6.2.0"
      ]

getHLSVersions :: Adw.ToastOverlay -> Adw.ApplicationWindow -> IO [Adw.ActionRow]
getHLSVersions toastOverlay app = traverse (toActionRow toastOverlay app "HLS") hlsVersions
  where
    hlsVersions =
      [ "1.8.0.0 (latest)"
      , "1.7.0.0"
      ]

toActionRow :: Adw.ToastOverlay -> Adw.ApplicationWindow -> Text -> Text -> IO Adw.ActionRow
toActionRow toastOverlay app toolLabel versionLabel = do
  installButton <-
    new
      Gtk.Switch
      [ #valign := Gtk.AlignCenter
      ]
  on installButton #stateSet $ \state -> do
    mockInstall installButton toastOverlay app state (toolLabel <> " " <> versionLabel)

  actionRow <-
    new
      Adw.ActionRow
      [ #title := versionLabel
      ]

  -- setToggle <- new Gtk.CheckButton
  --   [ #label := "Set as default"
  --   ]

  Adw.actionRowAddSuffix actionRow installButton
  -- Adw.actionRowAddSuffix actionRow setToggle
  pure actionRow
