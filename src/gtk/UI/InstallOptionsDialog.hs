module UI.InstallOptionsDialog
  ( present
  ) where

import Control.Exception (try)
import Control.Monad (void)
import Data.GI.Base
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import GHCup.Input.Parsers (uriParser)
import GHCup.Types (InstallDir (..), TargetVersionReq (..), tVerToText)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import Presentation.Row (RowSpec (..), toolShortName)
import Toolchain.Types (InstallOptions (..), defaultInstallOptions)

present :: Adw.ApplicationWindow -> RowSpec -> (InstallOptions -> IO ()) -> IO ()
present window spec onInstall = do
  let verb = if spec.installed then "Reinstall" else "Install"
      TargetVersionReq tv _ = spec.installReq
      heading = verb <> " " <> toolShortName spec.tool <> " " <> tVerToText tv

  isolateRef <- newIORef Nothing
  urlRef <- newIORef (Right Nothing)

  setRow <-
    new
      Adw.SwitchRow
      [ #title := "Set as default"
      , #subtitle := "Make this the active version after install"
      , #active := spec.isDefault
      ]
  forceRow <-
    new
      Adw.SwitchRow
      [ #title := "Force reinstall"
      , #subtitle := "Overwrite an existing installation"
      , #active := spec.installed
      ]

  isolateSubtitle <- newIORef ("Install into ghcup's own directory" :: Text)
  isolateRow <-
    new
      Adw.ActionRow
      [ #title := "Isolate to directory"
      , #subtitle := "Install into ghcup's own directory"
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

  let syncIsolation misolate = do
        writeIORef isolateRef misolate
        case misolate of
          Just path -> do
            isolateRow.setSubtitle (Text.pack path)
            clearButton.setVisible True
            setRow.setActive False
            setRow.setSensitive False
            setRow.setSubtitle "Isolated installs cannot be set as default"
            forceRow.setActive False
          Nothing -> do
            subtitle <- readIORef isolateSubtitle
            isolateRow.setSubtitle subtitle
            clearButton.setVisible False
            setRow.setSensitive True
            setRow.setSubtitle "Make this the active version after install"

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
              maybe (pure ()) (\p -> syncIsolation (Just p)) mpath
      )

  void $ on clearButton #clicked (syncIsolation Nothing)

  let validateUrl = do
        text <- Text.strip <$> urlRow.getText
        let outcome
              | Text.null text = Right Nothing
              | otherwise = either (Left . Text.pack) (Right . Just) (uriParser (Text.unpack text))
        writeIORef urlRef outcome
        case outcome of
          Left _ -> do
            urlRow.addCssClass "error"
            installButton.setSensitive False
          Right _ -> do
            urlRow.removeCssClass "error"
            installButton.setSensitive True

  void $ on urlRow #changed validateUrl

  void $ on cancelButton #clicked (dialog.forceClose)

  void $ on installButton #clicked $ do
    murl <- readIORef urlRef
    misolate <- readIORef isolateRef
    setActive <- setRow.getActive
    forceActive <- forceRow.getActive
    argsText <- argsRow.getText
    targetsText <- targetsRow.getText
    case murl of
      Left _ -> pure ()
      Right bindistUrl -> do
        let opts =
              defaultInstallOptions
                { setAsDefault = setActive
                , forceInstall = forceActive
                , installDir = maybe GHCupInternal IsolateDir misolate
                , bindistUrl
                , extraConfArgs = words (Text.unpack argsText)
                , installTargets = case words (Text.unpack targetsText) of
                    [] -> Nothing
                    ws -> Just ws
                }
        dialog.forceClose
        onInstall opts

  dialog.present (Just window)
