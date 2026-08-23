module UI.PreferencesWindow (present) where

import Control.Monad (void)
import Data.GI.Base
import GI.Adw qualified as Adw

import Config

present :: Adw.ApplicationWindow -> Config -> (ConfigUpdate -> IO ()) -> IO ()
present parent config onChanged = do
  toggle <-
    new
      Adw.SwitchRow
      [ #title := "Older Versions"
      , #subtitle := "List every version ghcup knows about"
      , #active := config.showOldVersions
      ]
  void $ on toggle (PropertyNotify #active) $ \_ -> do
    active <- toggle.getActive
    onChanged (SetShowOldVersions active)

  group <- new Adw.PreferencesGroup [#title := "Display"]
  group.add toggle
  page <- new Adw.PreferencesPage []
  page.add group
  window <-
    new
      Adw.PreferencesWindow
      [ #transientFor := parent
      , #modal := True
      ]
  window.add page
  window.present
