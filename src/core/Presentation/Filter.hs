module Presentation.Filter
  ( ActiveFilters (..)
  , Channel (..)
  , FilterKind (..)
  , baseLabel
  , channelLabel
  , channelsFor
  , filterLabel
  , filtersFor
  , reachableChannels
  , restrictTo
  , seedFilters
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHCup.Types (Tool, ghc)

import Toolchain.Channels (BaseChannel (..), Channel (..))

data FilterKind
  = ShowOldPatches
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data ActiveFilters = ActiveFilters
  { kinds :: Set FilterKind
  , channels :: Set Channel
  }
  deriving stock (Eq, Show)

instance Semigroup ActiveFilters where
  a <> b = ActiveFilters (a.kinds <> b.kinds) (a.channels <> b.channels)

instance Monoid ActiveFilters where
  mempty = ActiveFilters Set.empty Set.empty

filterLabel :: FilterKind -> Text
filterLabel = \case
  ShowOldPatches -> "Show older patch releases"

channelLabel :: Channel -> Text
channelLabel = \case
  Prereleases -> "Prereleases"
  Nightlies -> "Nightlies"
  Cross -> "Cross builds"
  ThirdParty -> "Third-party tools"

baseLabel :: BaseChannel -> Text
baseLabel = \case
  DefaultBase -> "Default"
  VanillaBase -> "Vanilla"

filtersFor :: [FilterKind]
filtersFor = [ShowOldPatches]

channelsFor :: Set Channel -> Tool -> [Channel]
channelsFor configured tool = filter (`Set.member` configured) available
  where
    available
      | tool == ghc = [Prereleases, Nightlies, Cross]
      | otherwise = [Prereleases]

-- | The configured channels these tools' filter bars can actually offer;
-- a channel no bar shows cannot warrant rebuilding any of them.
reachableChannels :: (Foldable f) => Set Channel -> f Tool -> Set Channel
reachableChannels configured = foldMap (Set.fromList . channelsFor configured)

seedFilters :: [Channel] -> [Channel] -> ActiveFilters -> ActiveFilters
seedFilters offeredBefore offeredNow carried =
  carried <> ActiveFilters Set.empty (Set.fromList offeredNow `Set.difference` Set.fromList offeredBefore)

-- | Keep only the selections a bar offering these kinds and channels can
-- represent; a filter naming something no longer on offer is dropped.
restrictTo :: [FilterKind] -> [Channel] -> ActiveFilters -> ActiveFilters
restrictTo kinds channels active =
  ActiveFilters
    (active.kinds `Set.intersection` Set.fromList kinds)
    (active.channels `Set.intersection` Set.fromList channels)
