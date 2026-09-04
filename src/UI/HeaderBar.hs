{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module UI.HeaderBar where

import Data.GI.Base
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import Control.Monad (void)

genHeaderbar :: (?self :: Adw.Application) => IO Adw.HeaderBar
genHeaderbar = do
    title <-
        new
            Adw.WindowTitle
            [ #title := "Haskell Toolchain Installer"
            ]

    headerBar <- new Adw.HeaderBar [#titleWidget := title]

    menuPopOver <- genMenuPopOver
    menuButton <- new Gtk.MenuButton [#iconName := "open-menu-symbolic"]
    Gtk.menuButtonSetPopover menuButton (Just menuPopOver)
    Adw.headerBarPackEnd headerBar menuButton
    pure headerBar

genMenuPopOver :: (?self :: Adw.Application) => IO Gtk.PopoverMenu
genMenuPopOver = do
    menu <- Gio.menuNew
    preferencesMenuItem <- Gio.menuItemNew (Just "Preferences") (Just "open-preferences")
    Gio.menuAppendItem menu preferencesMenuItem
    genAboutDialog
    void $ Gio.onApplicationActivate ?self genAboutDialog
    Gio.menuAppend menu (Just "About") (Just "app.open-about")
    Gtk.popoverMenuNewFromModel (Just menu)

genAboutDialog :: (?self :: Adw.Application) => IO ()
genAboutDialog = do
  action <- Gio.simpleActionNew "open-about" Nothing
  void $ Gio.onSimpleActionActivate action aboutDialog

aboutDialog :: Maybe a -> IO ()
aboutDialog _ = do
  dialog <- Adw.aboutWindowNew
  set dialog
    [ #version := "0.0.1.0"
    , #developerName := "Hécate Moonlight"
    , #licenseType := Gtk.LicenseBsd3
    , #comments := "Haskell Toolchain Manager"
    , #website := "https://www.haskell.org/ghcup/"
    , #copyright := "® 2023 Hécate Moonlight"
    ]
