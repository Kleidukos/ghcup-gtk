module UI.Preferences (present) where

import Control.Monad (void)
import Data.GI.Base
import GI.Adw qualified as Adw

import Config

present :: Adw.ApplicationWindow -> Config -> (ConfigUpdate -> IO ()) -> IO ()
present parent config onChanged = do
  advancedToggle <-
    new
      Adw.SwitchRow
      [ #title := "Advanced interface"
      , #subtitle := "Sortable table with release dates and filters"
      , #active := (config.viewMode == Advanced)
      ]

  void $ on advancedToggle (PropertyNotify #active) $ \_ -> do
    active <- advancedToggle.getActive
    onChanged (SetViewMode (if active then Advanced else Simple))

  interfaceGroup <- new Adw.PreferencesGroup [#title := "Interface"]
  interfaceGroup.add advancedToggle

  page <- new Adw.PreferencesPage []
  page.add interfaceGroup

  dialog <- new Adw.PreferencesDialog []
  dialog.add page
  dialog.present (Just parent)
