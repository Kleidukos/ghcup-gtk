module UI.InstallOptionsDialog
  ( present
  ) where

import Control.Exception (try)
import Control.Monad (forM_, void, when)
import Data.GI.Base
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import GHCup.Types (TargetVersionReq (..), tVerToText)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import Presentation.InstallForm (FormEvent (..), FormModel (..), canInstall, initFormModel, setDefaultLocked, stepForm, toOptions, urlError)
import Presentation.Row (RowSpec (..), installVerb, toolShortName)
import Toolchain.Types (InstallOptions)

defaultSubtitle :: Text
defaultSubtitle = "Make this the active version after install"

isolateSubtitle :: Text
isolateSubtitle = "Install into ghcup's own directory"

present :: Adw.ApplicationWindow -> RowSpec -> (InstallOptions -> IO ()) -> IO ()
present window spec onInstall = do
  let verb = installVerb spec
      TargetVersionReq tv _ = spec.installReq
      heading = verb <> " " <> toolShortName spec.tool <> " " <> tVerToText tv

  let initialModel = initFormModel spec
  modelRef <- newIORef initialModel

  setRow <-
    new
      Adw.SwitchRow
      [ #title := "Set as default"
      , #subtitle := defaultSubtitle
      , #active := initialModel.setDefault
      ]
  forceRow <-
    new
      Adw.SwitchRow
      [ #title := "Force reinstall"
      , #subtitle := "Overwrite an existing installation"
      , #active := initialModel.force
      ]

  isolateRow <-
    new
      Adw.ActionRow
      [ #title := "Isolate to directory"
      , #subtitle := isolateSubtitle
      ]
  pickButton <-
    new
      Gtk.Button
      [ #iconName := "folder-open-symbolic"
      , #valign := Gtk.AlignCenter
      , #cssClasses := ["flat"]
      , #tooltipText := "Choose a directory"
      ]
  clearButton <-
    new
      Gtk.Button
      [ #iconName := "edit-clear-symbolic"
      , #valign := Gtk.AlignCenter
      , #cssClasses := ["flat"]
      , #tooltipText := "Do not isolate"
      , #visible := False
      ]
  isolateRow.addSuffix pickButton
  isolateRow.addSuffix clearButton

  urlRow <- new Adw.EntryRow [#title := "Binary dist URL"]
  argsRow <- new Adw.EntryRow [#title := "Extra configure args"]
  targetsRow <- new Adw.EntryRow [#title := "Install targets"]

  group <- new Adw.PreferencesGroup []
  group.add =<< Gtk.toWidget setRow
  group.add =<< Gtk.toWidget forceRow
  group.add =<< Gtk.toWidget isolateRow
  group.add =<< Gtk.toWidget urlRow
  group.add =<< Gtk.toWidget argsRow
  group.add =<< Gtk.toWidget targetsRow

  clamp <-
    new
      Adw.Clamp
      [ #maximumSize := 480
      , #cssClasses := ["install-options-content"]
      ]
  groupWidget <- Gtk.toWidget group
  clamp.setChild (Just groupWidget)

  cancelButton <- new Gtk.Button [#label := "Cancel"]
  installButton <-
    new Gtk.Button [#label := verb, #cssClasses := ["suggested-action"]]

  header <-
    new
      Adw.HeaderBar
      [ #showStartTitleButtons := False
      , #showEndTitleButtons := False
      ]
  header.packStart cancelButton
  header.packEnd installButton

  toolbarView <- new Adw.ToolbarView []
  headerWidget <- Gtk.toWidget header
  toolbarView.addTopBar headerWidget
  clampWidget <- Gtk.toWidget clamp
  toolbarView.setContent (Just clampWidget)

  dialog <-
    new
      Adw.Dialog
      [ #title := heading
      , #contentWidth := 460
      ]
  toolbarWidget <- Gtk.toWidget toolbarView
  dialog.setChild (Just toolbarWidget)

  let render = do
        model <- readIORef modelRef
        case urlError model of
          Just _ -> urlRow.addCssClass "error"
          Nothing -> urlRow.removeCssClass "error"
        installButton.setSensitive (canInstall model)
        case model.isolate of
          Just path -> do
            isolateRow.setSubtitle (Text.pack path)
            clearButton.setVisible True
          Nothing -> do
            isolateRow.setSubtitle isolateSubtitle
            clearButton.setVisible False
        let locked = setDefaultLocked model
        setRow.setSensitive (not locked)
        setRow.setSubtitle
          (if locked then "Isolated installs cannot be set as default" else defaultSubtitle)
        setRow.setActive model.setDefault
        forceRow.setActive model.force

      dispatch event = do
        modifyIORef' modelRef (stepForm event)
        render

  void $ on urlRow #changed $ do
    text <- urlRow.getText
    dispatch (UrlChanged text)
  void $ on argsRow #changed $ do
    text <- argsRow.getText
    dispatch (ArgsChanged text)
  void $ on targetsRow #changed $ do
    text <- targetsRow.getText
    dispatch (TargetsChanged text)

  void $ on setRow (PropertyNotify #active) $ \_ -> do
    active <- setRow.getActive
    model <- readIORef modelRef
    when (active /= model.setDefault) $ dispatch (SetDefaultToggled active)
  void $ on forceRow (PropertyNotify #active) $ \_ -> do
    active <- forceRow.getActive
    model <- readIORef modelRef
    when (active /= model.force) $ dispatch (ForceToggled active)

  void $ on pickButton #clicked $ do
    fileDialog <- new Gtk.FileDialog [#title := "Isolate installation to…"]
    fileDialog.selectFolder
      (Just window)
      (Nothing @Gio.Cancellable)
      ( Just $ \_ asyncResult -> do
          result <- try @GError (fileDialog.selectFolderFinish asyncResult)
          case result of
            Left _dismissed -> pure ()
            Right file -> do
              mpath <- file.getPath
              forM_ mpath (dispatch . IsolatePicked)
      )

  void $ on clearButton #clicked (dispatch IsolateCleared)

  void $ on cancelButton #clicked dialog.forceClose

  void $ on installButton #clicked $ do
    model <- readIORef modelRef
    forM_ (toOptions model) $ \opts -> do
      dialog.forceClose
      onInstall opts

  dialog.present (Just window)
