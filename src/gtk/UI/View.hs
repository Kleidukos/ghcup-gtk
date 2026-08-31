module UI.View
  ( RowCallbacks (..)
  , View (..)
  , buildFilterBar
  , captionLabel
  , emptyStateStack
  , pillLabel
  ) where

import Control.Monad (filterM, forM_, unless, void)
import Data.GI.Base
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.Filter (ActiveFilters (..), Channel, FilterKind, activeCount, channelLabel, filterLabel, restrictTo)
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
  , getFilters :: IO ActiveFilters
  -- ^ The view's current selections, so a rebuild can carry them over
  }

-- | The filter funnel used by the list renderer: a menu button whose
-- popover holds one check button per filter, channels under their own
-- heading, with a pill showing how many are active. Returns the
-- selections it actually applied: filters naming a kind or channel this
-- bar does not offer are dropped, so the caller must adopt this value
-- rather than the 'ActiveFilters' it passed in.
buildFilterBar
  :: [FilterKind]
  -> [Channel]
  -> ActiveFilters
  -> (ActiveFilters -> IO ())
  -> IO (Gtk.Widget, ActiveFilters)
buildFilterBar kinds channels initial onChanged = do
  let active = restrictTo kinds channels initial
  kindChecks <- traverse (checkOf filterLabel (`Set.member` active.kinds)) kinds
  channelChecks <- traverse (checkOf channelLabel (`Set.member` active.channels)) channels

  list <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
  list.addCssClass "filter-popover-content"
  forM_ kindChecks $ \(_, check) -> list.append check
  unless (null channelChecks) $ do
    header <- new Gtk.Label [#label := "Channels", #xalign := 0]
    header.addCssClass "heading"
    header.addCssClass "filter-section-heading"
    list.append header
    forM_ channelChecks $ \(_, check) -> list.append check
  popover <- new Gtk.Popover [#child := list]

  icon <- new Gtk.Image [#iconName := "funnel-symbolic"]
  label <- new Gtk.Label [#label := "Filters"]
  badge <- pillLabel (countText (activeCount active))
  set badge [#visible := activeCount active > 0]

  content <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  content.append icon
  content.append label
  content.append badge

  button <- new Gtk.MenuButton [#popover := popover, #child := content]

  let activeSetOf checks = Set.fromList . map fst <$> filterM (\(_, check) -> check.getActive) checks
      currentFilters = ActiveFilters <$> activeSetOf kindChecks <*> activeSetOf channelChecks
  forM_ (map snd kindChecks <> map snd channelChecks) $ \check ->
    void $ on check #toggled $ do
      filters <- currentFilters
      let count = activeCount filters
      set badge [#label := countText count, #visible := count > 0]
      onChanged filters

  bar <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  bar.addCssClass "filter-bar"
  bar.append button
  barWidget <- Gtk.toWidget bar
  pure (barWidget, active)
  where
    checkOf labelOf isChecked kind = do
      check <- new Gtk.CheckButton [#label := labelOf kind, #active := isChecked kind]
      pure (kind, check)

    countText = Text.pack . show

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
