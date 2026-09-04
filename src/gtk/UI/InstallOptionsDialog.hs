module UI.InstallOptionsDialog
  ( present
  ) where

import Control.Monad (join, void)
import Data.GI.Base
import Data.IORef
import Data.Maybe (isNothing)
import Data.Text (Text)
import GHCup.Types (TargetVersionReq (..), tVerToText)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.InstallForm (FormEvent (..), FormModel (..), canInstall, effectiveForce, effectiveSetDefault, initFormModel, stepForm, toOptions, urlError)
import Presentation.Row (RowSpec (..), installVerb, toolShortName)
import Toolchain.Types (InstallOptions)
import UI.OptionsDialog

defaultSubtitle :: Text
defaultSubtitle = "Make this the active version after install"

present :: Adw.ApplicationWindow -> RowSpec -> (InstallOptions -> IO ()) -> IO ()
present window spec onInstall = do
  let verb = installVerb spec
      TargetVersionReq tv _ = spec.installReq
      heading = verb <> " " <> toolShortName spec.tool <> " " <> tVerToText tv

  modelRef <- newIORef (initFormModel spec)
  renderRef <- newIORef (pure ())

  let dispatch event = do
        modifyIORef' modelRef (stepForm event)
        join (readIORef renderRef)

  ui <-
    scaffold
      window
      ScaffoldSpec
        { heading
        , affirmLabel = verb
        , setSubtitle = defaultSubtitle
        , contentHeight = Nothing
        , onIsolatePicked = dispatch . IsolatePicked
        , onIsolateCleared = dispatch IsolateCleared
        }

  ui.group.add =<< Gtk.toWidget ui.setRow

  forceRow <-
    new
      Adw.SwitchRow
      [ #title := "Force reinstall"
      , #subtitle := "Overwrite an existing installation"
      ]
  ui.group.add =<< Gtk.toWidget forceRow

  ui.group.add =<< Gtk.toWidget ui.isolateRow

  urlRow <- entryRow ui.group "Binary dist URL" "" (dispatch . UrlChanged)
  void $ entryRow ui.group "Extra configure args" "" (dispatch . ArgsChanged)
  void $ entryRow ui.group "Install targets" "" (dispatch . TargetsChanged)

  writeIORef renderRef $ do
    model <- readIORef modelRef
    markError urlRow (urlError model)
    ui.affirmButton.setSensitive (canInstall model)
    renderIsolate ui model.isolate (effectiveSetDefault model)
    forceRow.setSensitive (isNothing model.isolate)
    forceRow.setActive (effectiveForce model)

  wireSetToggle ui (effectiveSetDefault <$> readIORef modelRef) (dispatch . SetDefaultToggled)
  wireSwitch forceRow (effectiveForce <$> readIORef modelRef) (dispatch . ForceToggled)
  wireAffirm ui (toOptions <$> readIORef modelRef) onInstall

  join (readIORef renderRef)
  ui.presentDialog
