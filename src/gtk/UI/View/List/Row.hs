module UI.View.List.Row
  ( RowHandle (..)
  , build
  ) where

import Control.Monad (forM_, when)
import Data.GI.Base
import Data.Maybe (isJust, isNothing)
import Data.Text.Display
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Presentation.Row (RowAction (..), RowSpec (..))
import Toolchain.Types (Progress (..))
import UI.Dialog qualified as Dialog
import UI.View (RowCallbacks (..), captionLabel, pillLabel)

data RowHandle = RowHandle
  { row :: Adw.ActionRow
  , defaultCheck :: Maybe Gtk.CheckButton
  }

build :: Adw.ApplicationWindow -> RowSpec -> RowCallbacks -> IO RowHandle
build window spec callbacks = do
  row <- new Adw.ActionRow [#title := spec.title]

  forM_ spec.pills $ \p -> do
    pill <- pillLabel (display p)
    row.addSuffix pill

  defaultCheck <-
    if spec.installed
      then do
        check <-
          new
            Gtk.CheckButton
            [ #label := "Default"
            , #valign := Gtk.AlignCenter
            , #active := spec.isDefault
            , #sensitive := not spec.isDefault
            ]
        on check #toggled $ do
          active <- check.getActive

          when (active && not spec.isDefault) $ callbacks.onSubmit spec.setDefault
        row.addSuffix check
        pure (Just check)
      else pure Nothing

  phaseLabel <-
    new
      Gtk.Label
      [ #valign := Gtk.AlignCenter
      , #visible := isJust spec.progress
      , #maxWidthChars := 30
      , #ellipsize := Pango.EllipsizeModeEnd
      ]
  captionLabel phaseLabel
  progressBar <-
    new Gtk.ProgressBar [#valign := Gtk.AlignCenter, #visible := isJust spec.progress]
  row.addSuffix phaseLabel
  row.addSuffix progressBar

  actionButton <-
    new
      Gtk.Button
      [ #label := spec.action.label
      , #valign := Gtk.AlignCenter
      , #visible := isNothing spec.progress
      ]
  on actionButton #clicked $
    Dialog.confirm window spec.action.confirmation $ \confirmed ->
      when confirmed $ callbacks.onSubmit spec.action.job
  row.addSuffix actionButton

  -- With a known fraction the bar is determinate; otherwise each redraw
  -- pulses once, the worker's progress ticks being the animation.
  forM_ spec.progress $ \progress -> do
    phaseLabel.setLabel progress.phase
    case progress.fraction of
      Just fraction -> progressBar.setFraction fraction
      Nothing -> progressBar.pulse

  pure RowHandle {row, defaultCheck}
