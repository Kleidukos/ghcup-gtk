module UI.View
  ( RowCallbacks (..)
  , View (..)
  , dimCaption
  , pillLabel
  ) where

import Data.GI.Base
import Data.Text (Text)
import GI.Gtk qualified as Gtk

import Presentation.Row (ToolRows)
import Toolchain.Types (Mutation)

newtype RowCallbacks = RowCallbacks
  { onSubmit :: Mutation -> IO ()
  }

data View = View
  { widget :: Gtk.Widget
  -- ^ The renderer's root widget
  , setRows :: RowCallbacks -> ToolRows -> IO ()
  -- ^ Replace the rendered rows
  , setSensitive :: Bool -> IO ()
  }

pillLabel :: Text -> IO Gtk.Label
pillLabel text = do
  pill <- new Gtk.Label [#label := text, #valign := Gtk.AlignCenter]
  dimCaption pill
  pure pill

-- | Small muted text. ".dim-label" was renamed ".dimmed" in libadwaita 1.7.
dimCaption :: Gtk.Label -> IO ()
dimCaption label = do
  label.addCssClass "caption"
  label.addCssClass "dim-label"
  label.addCssClass "dimmed"
