module Presentation.Filter
  ( FilterKind (..)
  , extraFiltersFor
  , filterLabel
  , filtersFor
  ) where

import Data.Text (Text)
import GHCup.Types (Tool, ghc)

data FilterKind
  = ShowOldPatches
  | ShowPrereleases
  | ShowNightlies
  | ShowCross
  deriving stock (Bounded, Enum, Eq, Ord, Show)

filterLabel :: FilterKind -> Text
filterLabel = \case
  ShowOldPatches -> "Show older patch releases"
  ShowPrereleases -> "Show prereleases"
  ShowNightlies -> "Show nightlies"
  ShowCross -> "Show cross builds"

filtersFor :: Tool -> [FilterKind]
filtersFor tool
  | tool == ghc = [ShowOldPatches]
  | otherwise = [ShowOldPatches, ShowPrereleases]

extraFiltersFor :: Tool -> [FilterKind]
extraFiltersFor tool
  | tool == ghc = [ShowPrereleases, ShowNightlies, ShowCross]
  | otherwise = []
