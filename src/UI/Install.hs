module UI.Install where

import Control.Monad (when)
import Data.GI.Base
import Data.Text (pack)
import Data.Versions (Version, prettyVer)
import GHCup.Types
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Driver.GHCup

install
  :: Gtk.Switch
  -> Adw.ToastOverlay
  -> Adw.ApplicationWindow
  -> AppState
  -> GHCupDownloads
  -> Bool
  -> Tool
  -> Version
  -> IO Bool
install selfSwitch toastOverlay app st ds state tool version = do
  curState <- selfSwitch.getState
  -- don't handle case when state is being reset: e.g. when 'Cancel'
  -- button is pressed on message box
  (True <$) $ when (curState /= state) $ do
    let stateText = if state then "install" else "uninstall"
        stateTextUpper = if state then "Install" else "Uninstall"
        messageText =
          "Are you sure you want to "
            <> stateText
            <> " "
            <> pack (show tool)
            <> " "
            <> prettyVer version
            <> "?"

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
            presentToast "Installing…"
            case tool of
              GHC -> do
                installGHC st ds version >>= \case
                  GHCupSuccess t -> do
                    presentToast "GHC installation successful"
                    maybe (pure ()) presentToast t
                  GHCupFailed e -> do
                    presentToast e
                    set selfSwitch [#active := not state] -- reset position
                set selfSwitch [#state := state]
              _ -> error "unsupported tool!"
          else do
            presentToast "Uninstalling…"
            case tool of
              GHC -> do
                rmGHC st ds version >>= \case
                  GHCupSuccess t -> do
                    presentToast "GHC removal successful"
                    maybe (pure ()) presentToast t
                  GHCupFailed e -> do
                    presentToast e
                    set selfSwitch [#active := not state] -- reset position
                set selfSwitch [#state := state]
      _ -> set selfSwitch [#active := not state] -- reset position
    messageDialog.present
  where
    presentToast title = do
      notificationToast <-
        new
          Adw.Toast
          [ #title := title
          , #timeout := 3
          ]
      Adw.toastOverlayAddToast toastOverlay notificationToast
