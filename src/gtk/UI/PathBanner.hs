module UI.PathBanner
  ( Handle (..)
  , build
  , render
  ) where

import Control.Monad (join, void, when)
import Data.GI.Base
import Data.IORef
import GI.Adw qualified as Adw

import Presentation.Path
import UI.Dialog qualified as Dialog

data Handle = Handle
  { window :: Adw.ApplicationWindow
  , widget :: Adw.Banner
  , onClick :: IORef (IO ())
  }

build :: Adw.ApplicationWindow -> IO Handle
build window = do
  widget <- new Adw.Banner [#revealed := False]
  onClick <- newIORef (pure ())
  void $ on widget #buttonClicked $ join (readIORef onClick)
  pure Handle {window, widget, onClick}

render :: Handle -> IO () -> Maybe BannerSpec -> IO ()
render handle onFixConfirmed = \case
  Nothing -> set handle.widget [#revealed := False]
  Just banner -> do
    -- An empty button label is how 'Adw.Banner' hides its button.
    let (buttonLabel, onClick) = case banner.action of
          Nothing -> ("", pure ())
          Just (ShowInfo spec) ->
            ( spec.buttonLabel
            , Dialog.info handle.window spec.dialogHeading spec.dialogBody
            )
          Just (ConfirmFix spec) ->
            ( spec.buttonLabel
            , Dialog.confirm handle.window spec.confirmation $ \confirmed ->
                when confirmed onFixConfirmed
            )
    writeIORef handle.onClick onClick
    set
      handle.widget
      [#title := banner.title, #buttonLabel := buttonLabel, #revealed := True]
