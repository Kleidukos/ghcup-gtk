module UI.Shortcuts (present) where

import Control.Monad (forM_)
import Data.GI.Base
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import UI.View (captionLabel)

-- | The shortcuts listed in the dialog, paired with the accelerators
-- registered in 'UI.installActions'.
shortcuts :: [(Text, Text)]
shortcuts =
  [ ("Preferences", "Ctrl+,")
  , ("Keyboard Shortcuts", "Ctrl+?")
  , ("Quit", "Ctrl+Q")
  ]

-- | Hand-built shortcuts dialog. 'Gtk.ShortcutsWindow' is deprecated and
-- AdwShortcutsDialog needs libadwaita 1.8, our minimum is 1.5.
present :: Adw.ApplicationWindow -> IO ()
present parent = do
  group <- new Adw.PreferencesGroup [#title := "General"]
  forM_ shortcuts $ \(title, accel) -> do
    row <- new Adw.ActionRow [#title := title]
    label <- new Gtk.Label [#label := accel, #valign := Gtk.AlignCenter]
    captionLabel label
    row.addSuffix label
    group.add row

  page <- new Adw.PreferencesPage []
  page.add group
  pageWidget <- Gtk.toWidget page

  header <- new Adw.HeaderBar []
  view <- new Adw.ToolbarView [#content := pageWidget]
  view.addTopBar header
  viewWidget <- Gtk.toWidget view

  dialog <-
    new
      Adw.Dialog
      [ #title := "Keyboard Shortcuts"
      , #child := viewWidget
      , #contentWidth := 420
      ]
  dialog.present (Just parent)
