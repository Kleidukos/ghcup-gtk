module UI.View
  ( FilterBar (..)
  , FiltersChanged
  , RowCallbacks (..)
  , View (..)
  , buildFilterBar
  , captionLabel
  , emptyStateStack
  , pillLabel
  ) where

import Control.Monad (filterM, forM_, void)
import Data.GI.Base
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHCup.Types (Tool)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Config (Config)
import Presentation.Filter (FilterKind, filterLabel)
import Presentation.Row (RowAction, ToolRows)
import Toolchain.Types (Mutation)

data RowCallbacks = RowCallbacks
  { onSubmit :: Mutation -> IO ()
  , onConfirm :: RowAction -> IO ()
  }

data View = View
  { widget :: Gtk.Widget
  -- ^ The renderer's root widget
  , setRows :: ToolRows -> IO ()
  -- ^ Replace the rendered rows
  , setSensitive :: Bool -> IO ()
  , applyConfig :: Config -> IO ()
  }

type FiltersChanged = Tool -> Set FilterKind -> IO ()

-- | The filter checkboxes shared by the list and table renderers.
data FilterBar = FilterBar
  { widget :: Gtk.Widget
  , setFilters :: Set FilterKind -> IO ()
  }

buildFilterBar :: [FilterKind] -> Set FilterKind -> (Set FilterKind -> IO ()) -> IO FilterBar
buildFilterBar kinds initial onChanged = do
  checks <- traverse checkOf kinds
  bar <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 12
      ]
  bar.addCssClass "filter-bar"
  forM_ checks $ \(_, check) -> bar.append check
  let currentFilters =
        Set.fromList . map fst <$> filterM (\(_, check) -> check.getActive) checks
  forM_ checks $ \(_, check) ->
    void $ on check #toggled (currentFilters >>= onChanged)
  widget <- Gtk.toWidget bar
  let setFilters filters =
        forM_ checks $ \(kind, check) -> check.setActive (Set.member kind filters)
  pure FilterBar {widget, setFilters}
  where
    checkOf kind = do
      check <- new Gtk.CheckButton [#label := filterLabel kind, #active := Set.member kind initial]
      pure (kind, check)

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
