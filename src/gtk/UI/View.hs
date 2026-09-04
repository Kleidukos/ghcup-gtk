module UI.View
  ( RowCallbacks (..)
  , View (..)
  , buildFilterBar
  , captionLabel
  , emptyStateStack
  , pillLabel
  ) where

import Control.Monad (filterM, forM_, void, when)
import Data.Functor ((<&>))
import Data.GI.Base
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.Filter (ActiveFilters (..), Channel, FilterKind, baseLabel, channelLabel, filterLabel, restrictTo)
import Presentation.Row (RowAction, ToolRows)
import Session (ChannelsEditability (..))
import Toolchain.Channels (BaseChannel (..))
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

buildFilterBar
  :: [FilterKind]
  -> [Channel]
  -> BaseChannel
  -> ChannelsEditability
  -> (BaseChannel -> IO ())
  -> ActiveFilters
  -> (ActiveFilters -> IO ())
  -> IO (Gtk.Widget, ActiveFilters)
buildFilterBar kinds channels base editable onBaseChanged initial onChanged = do
  let active = restrictTo kinds channels initial
  kindChecks <- traverse (checkOf filterLabel (`Set.member` active.kinds)) kinds
  channelChecks <- traverse (checkOf channelLabel (`Set.member` active.channels)) channels

  (kindsButton, kindsBadge) <- menuButton "funnel-symbolic" "Filters" [] (map snd kindChecks)
  (channelsButton, channelsBadge) <-
    channelsMenuButton base editable onBaseChanged (map snd channelChecks)

  bar <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  bar.addCssClass "filter-bar"
  bar.append kindsButton
  bar.append channelsButton

  let activeSetOf checks = Set.fromList . map fst <$> filterM (\(_, check) -> check.getActive) checks
      currentFilters = ActiveFilters <$> activeSetOf kindChecks <*> activeSetOf channelChecks
      updateBadges filters = do
        let kindsCount = Set.size filters.kinds
            channelCount = Set.size filters.channels
        set kindsBadge [#label := countText kindsCount, #visible := kindsCount > 0]
        set channelsBadge [#label := countText channelCount, #visible := channelCount > 0]

  updateBadges active
  forM_ (map snd kindChecks <> map snd channelChecks) $ \check ->
    void $ on check #toggled $ do
      filters <- currentFilters
      updateBadges filters
      onChanged filters

  barWidget <- Gtk.toWidget bar
  pure (barWidget, active)
  where
    checkOf labelOf isChecked kind = do
      check <- new Gtk.CheckButton [#label := labelOf kind, #active := isChecked kind]
      pure (kind, check)

    countText = Text.pack . show

menuButton
  :: Text
  -> Text
  -> [Gtk.Widget]
  -> [Gtk.CheckButton]
  -> IO (Gtk.MenuButton, Gtk.Label)
menuButton iconName label topRows checks = do
  list <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
  list.addCssClass "filter-popover-content"
  forM_ topRows list.append
  forM_ checks list.append
  popover <- new Gtk.Popover [#child := list]

  content <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  new Gtk.Image [#iconName := iconName] >>= content.append
  labelWidget <- new Gtk.Label [#label := label]
  badge <- pillLabel ""
  content.append labelWidget
  content.append badge

  button <- new Gtk.MenuButton [#popover := popover, #child := content]
  pure (button, badge)

channelsMenuButton
  :: BaseChannel
  -> ChannelsEditability
  -> (BaseChannel -> IO ())
  -> [Gtk.CheckButton]
  -> IO (Gtk.MenuButton, Gtk.Label)
channelsMenuButton currentBase editability onBase checks = do
  let sensitive = editability == ChannelsEditable
  defaultRadio <-
    new Gtk.CheckButton [#label := baseLabel DefaultBase, #active := False, #sensitive := sensitive]
  vanillaRadio <-
    new Gtk.CheckButton [#label := baseLabel VanillaBase, #active := False, #sensitive := sensitive]
  vanillaRadio.setGroup (Just defaultRadio)
  case currentBase of
    DefaultBase -> defaultRadio.setActive True
    VanillaBase -> vanillaRadio.setActive True

  radioRows <- traverse Gtk.toWidget [defaultRadio, vanillaRadio]
  separatorRow <-
    if null checks
      then pure []
      else do
        separator <- new Gtk.Separator [#orientation := Gtk.OrientationHorizontal]
        Gtk.toWidget separator <&> pure
  (button, badge) <-
    menuButton "network-workgroup-symbolic" "Channels" (radioRows <> separatorRow) checks

  let radioToggled radio target = void $ on radio #toggled $ do
        isActive <- radio.getActive
        when (isActive && target /= currentBase) (onBase target)
  radioToggled defaultRadio DefaultBase
  radioToggled vanillaRadio VanillaBase

  pure (button, badge)

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
