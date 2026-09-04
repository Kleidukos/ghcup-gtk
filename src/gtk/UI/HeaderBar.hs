module UI.HeaderBar (genHeaderbar) where

import Data.GI.Base
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

genHeaderbar :: IO Adw.HeaderBar
genHeaderbar = do
  headerBar <- Adw.headerBarNew
  menu <- Gio.menuNew
  Gio.menuAppend menu (Just "Preferences") (Just "app.preferences")
  Gio.menuAppend menu (Just "About ghcup-gtk") (Just "app.about")
  menuButton <-
    new
      Gtk.MenuButton
      [ #iconName := "open-menu-symbolic"
      , #tooltipText := "Main Menu"
      ]
  popover <- Gtk.popoverMenuNewFromModel (Just menu)
  Gtk.menuButtonSetPopover menuButton (Just popover)
  Adw.headerBarPackEnd headerBar menuButton
  pure headerBar
