{-# LANGUAGE ImplicitParams #-}

module UI.HeaderBar where

import Control.Monad (void)
import Data.GI.Base
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import UI.PreferencesWindow

genHeaderbar :: (?self :: Adw.Application) => IO Adw.HeaderBar
genHeaderbar = do
  headerBar <- Adw.headerBarNew
  menuPopOver <- genMenuPopOver
  menuButton <- new Gtk.MenuButton [#iconName := "open-menu-symbolic"]
  Gtk.menuButtonSetPopover menuButton (Just menuPopOver)
  Adw.headerBarPackEnd headerBar menuButton
  pure headerBar

genMenuPopOver :: (?self :: Adw.Application) => IO Gtk.PopoverMenu
genMenuPopOver = do
  menu <- Gio.menuNew
  genAboutWindow
  genPreferencesWindow
  Gio.menuAppend menu (Just "Preferences") (Just "app.open-preferences")
  Gio.menuAppend menu (Just "About") (Just "app.open-about")
  Gtk.popoverMenuNewFromModel (Just menu)

genAboutWindow :: (?self :: Adw.Application) => IO ()
genAboutWindow = do
  action <- Gio.simpleActionNew "open-about" Nothing
  aboutWindow <- mkAboutWindow
  void $ Gio.onSimpleActionActivate action $ \_ ->
    Gtk.widgetShow aboutWindow
  Gio.actionMapAddAction ?self action
  where
    mkAboutWindow :: IO Adw.AboutWindow
    mkAboutWindow =
      new
        Adw.AboutWindow
        [ #applicationName := "Haskell Toolchain Manager"
        , #version := "0.0.1.0"
        , #developerName := "by Hécate Moonlight"
        , #licenseType := Gtk.LicenseBsd3
        , #comments := "A GTK4 frontend for the ghcup toolchain manager"
        , #website := "https://www.haskell.org/ghcup/"
        , #copyright := "® 2023 Hécate Moonlight"
        , #modal := True
        ]
