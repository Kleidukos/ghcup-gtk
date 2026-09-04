{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}

module UI.PreferencesWindow where

import Control.Monad (void)
import Data.GI.Base
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

genPreferencesWindow :: (?self :: Adw.Application) => IO ()
genPreferencesWindow = do
  action <- Gio.simpleActionNew "open-preferences" Nothing
  preferencesWindow <- mkPreferencesWindow
  programsPreferencesPage <- mkProgramsPreferences
  Adw.preferencesWindowAdd preferencesWindow programsPreferencesPage
  void $ Gio.onSimpleActionActivate action $ \_ ->
    Gtk.widgetShow preferencesWindow
  Gio.actionMapAddAction ?self action
  where
    mkPreferencesWindow :: IO Adw.PreferencesWindow
    mkPreferencesWindow =
      new
        Adw.PreferencesWindow
        [ #canNavigateBack := True
        , #searchEnabled := True
        ]
    mkProgramsPreferences :: IO Adw.PreferencesPage
    mkProgramsPreferences = do
      curlOptions <-
        new
          Adw.EntryRow
          [ #inputPurpose := Gtk.InputPurposeFreeForm
          ]
      curlGroup <-
        new
          Adw.PreferencesGroup
          [ #title := "cURL Options"
          ]
      Adw.preferencesGroupAdd curlGroup curlOptions

      wgetOptions <-
        new
          Adw.EntryRow
          [ #inputPurpose := Gtk.InputPurposeFreeForm
          ]

      wgetGroup <-
        new
          Adw.PreferencesGroup
          [ #title := "wget Options"
          ]

      Adw.preferencesGroupAdd wgetGroup wgetOptions

      preferencesPage <-
        new
          Adw.PreferencesPage
          [ #title := "Third-party programs options"
          ]

      Adw.preferencesPageAdd preferencesPage wgetGroup
      Adw.preferencesPageAdd preferencesPage curlGroup

      pure preferencesPage
