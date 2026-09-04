module UI.Install where

import Control.Monad (when)
import Data.GI.Base
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

mockInstall :: Gtk.Switch -> Adw.ToastOverlay -> Adw.ApplicationWindow -> Bool -> Text -> IO Bool
mockInstall selfSwitch toastOverlay app state toolDescription = do
  curState <- selfSwitch.getState
  -- don't handle case when state is being reset: e.g. when 'Cancel'
  -- button is pressed on message box
  (True<$) $ when (curState /= state) $ do
    let stateText = if state then "install" else "uninstall"
        stateTextUpper = if state then "Install" else "Uninstall"
        messageText =
          "Are you sure you want to " <>
          stateText <> " " <>
          toolDescription <> "?"

    messageDialog <-
      Adw.messageDialogNew
      (Just app)
      (Just $ "Confirm " <> stateText)
      (Just messageText)
    messageDialog.addResponse "cancel" "Cancel"
    messageDialog.addResponse "doit" stateTextUpper

    messageDialog.setResponseAppearance "doit" Adw.ResponseAppearanceDestructive
    messageDialog.setDefaultResponse (Just "cancel")
    messageDialog.setCloseResponse "cancel"

    on messageDialog #response $ \case
      "doit" ->
        if state
          then do
            notificationToast <-
              new
                Adw.Toast
                [ #title := "Installing…"
                , #timeout := 3
                ]
            Adw.toastOverlayAddToast toastOverlay notificationToast
            set selfSwitch [#state := state]
          else do
            notificationToast <-
              new
                Adw.Toast
                [ #title := "Uninstalling…"
                , #timeout := 3
                ]
            Adw.toastOverlayAddToast toastOverlay notificationToast
            set selfSwitch [#state := state]
      _ -> set selfSwitch [#active := not state]  -- reset position

    messageDialog.present
