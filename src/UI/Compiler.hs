module UI.Compiler where

import Data.GI.Base
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import UI.Install

getGHCVersions :: Adw.ToastOverlay -> IO [Adw.ActionRow]
getGHCVersions toastOverlay = traverse (toActionRow toastOverlay) compilerList
  where
    compilerList =
        [ "9.4.3 (latest)"
        , "9.2.5"
        , "9.0.2"
        , "8.10.7"
        ]

getCabalVersions :: Adw.ToastOverlay -> IO [Adw.ActionRow]
getCabalVersions toastOverlay = traverse (toActionRow toastOverlay) cabalVersions
  where
    cabalVersions =
        [ "3.8.1.0 (latest)"
        , "3.6.2.0"
        ]

getHLSVersions :: Adw.ToastOverlay -> IO [Adw.ActionRow]
getHLSVersions toastOverlay = traverse (toActionRow toastOverlay) hlsVersions
  where
    hlsVersions =
        [ "1.8.0.0 (latest)"
        , "1.7.0.0"
        ]

toActionRow :: Adw.ToastOverlay -> Text -> IO Adw.ActionRow
toActionRow toastOverlay compilerLabel = do
    installButton <-
        new
            Gtk.Switch
            [ #valign := Gtk.AlignCenter
            ]
    on installButton #stateSet $ \state -> do
        mockInstall toastOverlay state

    actionRow <-
        new
            Adw.ActionRow
            [ #title := compilerLabel
            ]

    -- setToggle <- new Gtk.CheckButton
    --   [ #label := "Set as default"
    --   ]

    Adw.actionRowAddSuffix actionRow installButton
    -- Adw.actionRowAddSuffix actionRow setToggle
    pure actionRow
