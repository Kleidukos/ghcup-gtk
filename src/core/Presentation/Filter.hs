module Presentation.Filter
  ( FilterKind (..)
  , ToolFilters
  , activeFilters
  , defaultFilters
  , filterFromName
  , filterLabel
  , filterName
  , filtersFor
  ) where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

filterName :: FilterKind -> Text
filterName = \case
  HlsPoweredOnly -> "hls-powered"
  LatestPatchOnly -> "latest-patch"
  ShowPrereleases -> "show-prereleases"
  ShowNightlies -> "show-nightlies"
  ShowCross -> "show-cross"

filterFromName :: Text -> Maybe FilterKind
filterFromName name = List.find (\kind -> filterName kind == name) [minBound .. maxBound]

filtersFor :: Tool -> [FilterKind]
filtersFor tool
  | tool == ghc = [minBound .. maxBound]
  | otherwise = [LatestPatchOnly, ShowPrereleases]

defaultFilters :: Tool -> Set FilterKind
defaultFilters tool
  | tool == ghc = Set.fromList [HlsPoweredOnly, LatestPatchOnly]
  | otherwise = Set.singleton LatestPatchOnly

type ToolFilters = Map Tool (Set FilterKind)

activeFilters :: Tool -> ToolFilters -> Set FilterKind
activeFilters tool = Map.findWithDefault (defaultFilters tool) tool
