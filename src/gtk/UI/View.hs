module UI.View
  ( FilterBar (..)
  , RowCallbacks (..)
  , View (..)
  , buildFilterBar
  , captionLabel
  , emptyStateStack
  , pillLabel
  ) where

import Control.Monad (void)
import Data.GI.Base
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Config (Filters (..))
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

-- | The filter checkboxes shared by the list and table renderers.
data FilterBar = FilterBar
  { widget :: Gtk.Widget
  , setFilters :: Filters -> IO ()
  }

buildFilterBar :: Filters -> (Filters -> IO ()) -> IO FilterBar
buildFilterBar initialFilters onChanged = do
  hlsCheck <-
    new Gtk.CheckButton [#label := "HLS-powered", #active := initialFilters.hlsPoweredOnly]
  latestCheck <-
    new
      Gtk.CheckButton
      [ #label := "Latest patch per major.minor"
      , #active := initialFilters.latestPatchOnly
      ]
  bar <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 12
      , #marginStart := 12
      , #marginEnd := 12
      ]
  bar.addCssClass "filter-bar"
  bar.append hlsCheck
  bar.append latestCheck

  let currentFilters = Filters <$> hlsCheck.getActive <*> latestCheck.getActive
  void $ on hlsCheck #toggled (currentFilters >>= onChanged)
  void $ on latestCheck #toggled (currentFilters >>= onChanged)

  widget <- Gtk.toWidget bar
  let setFilters filters = do
        hlsCheck.setActive filters.hlsPoweredOnly
        latestCheck.setActive filters.latestPatchOnly
  pure FilterBar {widget, setFilters}

emptyStateStack :: Gtk.Widget -> IO (Gtk.Stack, Bool -> IO ())
emptyStateStack content = do
  emptyPage <-
    new
      Adw.StatusPage
      [ #title := "No versions match the filters"
      , #iconName := "system-search-symbolic"
      ]
  stack <- new Gtk.Stack []
  stack.addNamed content (Just "rows")
  stack.addNamed emptyPage (Just "empty")
  let setEmpty isEmpty = stack.setVisibleChildName (if isEmpty then "empty" else "rows")
  pure (stack, setEmpty)

pillLabel :: Text -> IO Gtk.Label
pillLabel text = do
  pill <-
    new
      Gtk.Label
      [ #label := text
      , #valign := Gtk.AlignCenter
      ]
  pill.addCssClass "round-pill"
  captionLabel pill
  pure pill

captionLabel :: Gtk.Label -> IO ()
captionLabel label = do
  label.addCssClass "caption"
