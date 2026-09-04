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
import Toolchain.Types (Mutation, Progress, RowKey)

newtype RowCallbacks = RowCallbacks
  { onSubmit :: Mutation -> IO ()
  }

-- | One renderer for a tool's version list. 'UI.Registry' talks to a
-- renderer only through this record, so the simple list and the advanced
-- table are interchangeable and neither knows about the other.
data View = View
  { widget :: Gtk.Widget
  -- ^ The renderer's root widget, added to its tool pane's stack once.
  , setRows :: RowCallbacks -> ToolRows -> IO ()
  -- ^ Replace the rendered rows. Called only for the visible renderer.
  , setBusy :: RowKey -> Progress -> IO ()
  , setIdle :: RowKey -> IO ()
  , setSensitive :: Bool -> IO ()
  }

-- | A version tag ("latest", "recommended") rendered the same way in every
-- renderer; the caller attaches it.
pillLabel :: Text -> IO Gtk.Label
pillLabel text = do
  pill <- new Gtk.Label [#label := text, #valign := Gtk.AlignCenter]
  dimCaption pill
  pure pill

-- | Small muted text. ".dim-label" was renamed ".dimmed" in libadwaita 1.7;
-- setting both keeps the muted style on either side of our 1.5 floor.
dimCaption :: Gtk.Label -> IO ()
dimCaption label = do
  label.addCssClass "caption"
  label.addCssClass "dim-label"
  label.addCssClass "dimmed"
