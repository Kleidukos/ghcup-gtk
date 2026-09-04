module UI.PreferencesWindow (present) where

import Control.Monad (void)
import Data.GI.Base
import Data.Text (Text)
import GI.Adw qualified as Adw

import Config

present :: Adw.ApplicationWindow -> Config -> (ConfigUpdate -> IO ()) -> IO ()
present parent config onChanged = do
  advancedToggle <-
    new
      Adw.SwitchRow
      [ #title := "Advanced interface"
      , #subtitle := "Sortable table with release dates and filters"
      , #active := config.advancedInterface
      ]

  oldVersionsToggle <-
    new
      Adw.SwitchRow
      [ #title := "Older Versions"
      , #subtitle := oldVersionsSubtitle config.advancedInterface
      , #active := config.showOldVersions
      , #sensitive := not config.advancedInterface
      ]

  void $ on advancedToggle (PropertyNotify #active) $ \_ -> do
    active <- advancedToggle.getActive
    set
      oldVersionsToggle
      [ #sensitive := not active
      , #subtitle := oldVersionsSubtitle active
      ]
    onChanged (SetAdvancedInterface active)

  void $ on oldVersionsToggle (PropertyNotify #active) $ \_ -> do
    active <- oldVersionsToggle.getActive
    onChanged (SetShowOldVersions active)

  interfaceGroup <- new Adw.PreferencesGroup [#title := "Interface"]
  interfaceGroup.add advancedToggle
  displayGroup <- new Adw.PreferencesGroup [#title := "Display"]
  displayGroup.add oldVersionsToggle

  page <- new Adw.PreferencesPage []
  page.add interfaceGroup
  page.add displayGroup

  window <- new Adw.PreferencesWindow [#transientFor := parent, #modal := True]
  window.add page
  window.present

oldVersionsSubtitle :: Bool -> Text
oldVersionsSubtitle advanced
  | advanced = "Not used by the advanced interface"
  | otherwise = "List every version ghcup knows about"
