module UI.Banner
  ( Handle (..)
  , Callbacks (..)
  , build
  , render
  ) where

import Control.Monad (forM_, when)
import Data.GI.Base
import Data.IORef
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Presentation.Banner
import UI.Dialog qualified as Dialog

data Handle = Handle
  { window :: Adw.ApplicationWindow
  , widget :: Gtk.Revealer
  , title :: Gtk.Label
  , buttons :: Gtk.Box
  , shown :: IORef (Maybe BannerSpec)
  }

data Callbacks = Callbacks
  { onFixConfirmed :: IO ()
  , onDismiss :: IO ()
  }

build :: Adw.ApplicationWindow -> IO Handle
build window = do
  title <-
    new
      Gtk.Label
      [ #hexpand := True
      , #xalign := 0
      , #wrap := True
      , #wrapMode := Pango.WrapModeWordChar
      , #selectable := True
      ]
  title.addCssClass "heading"
  buttons <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 6
      , #valign := Gtk.AlignCenter
      ]
  content <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  content.append title
  content.append buttons
  widget <-
    new
      Gtk.Revealer
      [ #child := content
      , #transitionType := Gtk.RevealerTransitionTypeSlideDown
      , #revealChild := False
      ]
  widget.addCssClass "app-banner"
  shown <- newIORef Nothing
  pure Handle {window, widget, title, buttons, shown}

render :: Handle -> Callbacks -> Maybe BannerSpec -> IO ()
render handle callbacks spec = do
  previous <- readIORef handle.shown
  when (previous /= spec) $ do
    writeIORef handle.shown spec
    case spec of
      Nothing -> set handle.widget [#revealChild := False]
      Just banner -> do
        removeChildren handle.buttons
        forM_ banner.actions $ \action -> do
          let (label, onClick) = actionButton handle callbacks action
          button <- new Gtk.Button [#label := label, #valign := Gtk.AlignCenter, #focusOnClick := False]
          on button #clicked onClick
          handle.buttons.append button
        set handle.title [#label := banner.title]
        set handle.widget [#revealChild := True]

actionButton :: Handle -> Callbacks -> BannerAction -> (Text, IO ())
actionButton handle callbacks = \case
  ShowInfo spec ->
    (spec.buttonLabel, Dialog.info handle.window spec.dialogHeading spec.dialogBody)
  ConfirmFix spec ->
    ( spec.buttonLabel
    , Dialog.confirm handle.window spec.confirmation $ \confirmed ->
        when confirmed callbacks.onFixConfirmed
    )
  OpenLog path -> ("Open log", Dialog.openFile handle.window path)
  Dismiss -> ("Dismiss", callbacks.onDismiss)

removeChildren :: Gtk.Box -> IO ()
removeChildren box =
  Gtk.widgetGetFirstChild box >>= \case
    Nothing -> pure ()
    Just child -> box.remove child >> removeChildren box
