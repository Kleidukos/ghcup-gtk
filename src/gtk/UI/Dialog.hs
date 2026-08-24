module UI.Dialog
  ( info
  , confirm
  ) where

import Data.GI.Base
import Data.Text (Text)
import GI.Adw qualified as Adw

import Presentation.Row (Confirmation (..))

info :: Adw.ApplicationWindow -> Text -> Text -> IO ()
info window heading body = do
  dialog <- Adw.alertDialogNew (Just heading) (Just body)
  dialog.addResponse "ok" "Close"
  dialog.present (Just window)

confirm :: Adw.ApplicationWindow -> Confirmation -> (Bool -> IO ()) -> IO ()
confirm window spec done = do
  dialog <- Adw.alertDialogNew (Just spec.heading) (Just spec.body)
  dialog.addResponse "cancel" "Cancel"
  dialog.addResponse "doit" spec.affirmLabel
  if spec.destructive
    then do
      dialog.setResponseAppearance "doit" Adw.ResponseAppearanceDestructive
      dialog.setDefaultResponse (Just "cancel")
    else do
      dialog.setResponseAppearance "doit" Adw.ResponseAppearanceSuggested
      dialog.setDefaultResponse (Just "doit")
  dialog.setCloseResponse "cancel"
  on dialog #response $ \response -> done (response == "doit")
  dialog.present (Just window)
