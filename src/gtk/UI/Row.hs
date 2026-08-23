module UI.Row
  ( RowCallbacks (..)
  , RowHandle (..)
  , build
  ) where

import Control.Monad (forM_, when)
import Data.GI.Base
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Presentation (RowAction (..), RowSpec (..))
import Toolchain.Types (Mutation, Progress (..))
import UI.Dialog qualified as Dialog

newtype RowCallbacks = RowCallbacks
  { onSubmit :: Mutation -> IO ()
  }

data RowHandle = RowHandle
  { row :: Adw.ActionRow
  , setBusy :: Progress -> IO ()
  , setIdle :: IO ()
  , defaultCheck :: Maybe Gtk.CheckButton
  }

build :: Adw.ApplicationWindow -> RowSpec -> RowCallbacks -> IO RowHandle
build window spec callbacks = do
  row <- new Adw.ActionRow [#title := spec.title]

  forM_ spec.pills $ \text -> do
    pill <- new Gtk.Label [#label := text, #valign := Gtk.AlignCenter]
    pill.addCssClass "caption"
    pill.addCssClass "dim-label"
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
      , #visible := False
      , #maxWidthChars := 30
      , #ellipsize := Pango.EllipsizeModeEnd
      ]
  phaseLabel.addCssClass "caption"
  phaseLabel.addCssClass "dim-label"
  progressBar <-
    new Gtk.ProgressBar [#valign := Gtk.AlignCenter, #visible := False]
  row.addSuffix phaseLabel
  row.addSuffix progressBar

  actionButton <-
    new
      Gtk.Button
      [ #label := spec.action.label
      , #valign := Gtk.AlignCenter
      ]
  on actionButton #clicked $
    Dialog.confirm window spec.action.confirmation $ \confirmed ->
      when confirmed $ callbacks.onSubmit spec.action.job
  row.addSuffix actionButton

  let setBusy progress = do
        actionButton.setVisible False
        progressBar.setVisible True
        phaseLabel.setVisible True
        phaseLabel.setLabel progress.phase
        progressBar.pulse
      setIdle = do
        progressBar.setVisible False
        phaseLabel.setVisible False
        actionButton.setVisible True

  pure RowHandle {row, setBusy, setIdle, defaultCheck}
