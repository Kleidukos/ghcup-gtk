{-# LANGUAGE ImplicitParams #-}

module UI.Install where

import Data.GI.Base
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

mockInstall :: (?self :: Gtk.Switch) => Adw.ToastOverlay -> Bool -> IO Bool
mockInstall toastOverlay state = do
  if state
    then do
      notificationToast <-
        new
          Adw.Toast
          [ #title := "Installing…"
          , #timeout := 3
          ]
      Adw.toastOverlayAddToast toastOverlay notificationToast
      set ?self [#state := state]
      pure True
    else do
      notificationToast <-
        new
          Adw.Toast
          [ #title := "Uninstalling…"
          , #timeout := 3
          ]
      Adw.toastOverlayAddToast toastOverlay notificationToast
      set ?self [#state := state]
      pure True
