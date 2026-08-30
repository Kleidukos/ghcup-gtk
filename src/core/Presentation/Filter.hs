module Presentation.Filter
  ( FilterKind (..)
  , advancedFiltersFor
  , defaultFilters
  , filterLabel
  , filtersFor
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHCup.Types (Tool, ghc)

data FilterKind
  = HlsPoweredOnly
  | LatestPatchOnly
  | ShowPrereleases
  | ShowNightlies
  | ShowCross
  deriving stock (Bounded, Enum, Eq, Ord, Show)

filterLabel :: FilterKind -> Text
filterLabel = \case
  HlsPoweredOnly -> "HLS-powered"
  LatestPatchOnly -> "Latest patch per major.minor"
  ShowPrereleases -> "Show prereleases"
  ShowNightlies -> "Show nightlies"
  ShowCross -> "Show cross builds"

filtersFor :: Tool -> [FilterKind]
filtersFor tool
  | tool == ghc = [HlsPoweredOnly, LatestPatchOnly]
  | otherwise = [LatestPatchOnly, ShowPrereleases]

advancedFiltersFor :: Tool -> [FilterKind]
advancedFiltersFor tool
  | tool == ghc = [minBound .. maxBound]
  | otherwise = filtersFor tool

defaultFilters :: Tool -> Set FilterKind
defaultFilters tool
  | tool == ghc = Set.fromList [HlsPoweredOnly, LatestPatchOnly]
  | otherwise = Set.singleton LatestPatchOnly
