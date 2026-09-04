module UI.View.List.Row
  ( build
  ) where

import Control.Monad (forM_)
import Data.GI.Base
import Data.Text.Display
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.Row (RowSpec (..))
import UI.View (RowCallbacks, pillLabel)
import UI.View.ActionStrip qualified as ActionStrip

build :: Adw.ApplicationWindow -> Gtk.CheckButton -> RowSpec -> RowCallbacks -> IO Adw.ActionRow
build window defaultGroup spec callbacks = do
  row <- new Adw.ActionRow [#title := spec.title]

  forM_ spec.pills $ \p -> do
    pill <- pillLabel (display p)
    row.addSuffix pill

  strip <- ActionStrip.build window callbacks defaultGroup 30 spec
  row.addSuffix strip
  pure row
