module Toolchain.Curation
  ( FamilyKey
  , Stability (..)
  , curate
  , familyKey
  , isLatestInFamily
  , latestPerFamily
  , stabilityOf
  ) where

import Data.Functor
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Versions (Chunk (..), Chunks (..), Version (..))
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..))

import Toolchain.Types (Listings)

-- | Hide rows that cannot be installed, sort the rest newest-first.
-- Narrowing the list further is the views' job, via their filter bars.
curate :: Listings -> Listings
curate listings =
  listings
    <&> Vector.filter installable
    <&> Vector.toList
    <&> List.sortOn (Down . lVer)
    <&> Vector.fromList
  where
    installable lr = lInstalled lr || not (lNoBindist lr)

-- | A release family: same cross-compilation target, Major.Minor version,
-- and stability. Stability is in the key so that a prerelease or nightly,
-- whose version outranks the stable release, does not hide it as the latest.
type FamilyKey = (Maybe Text, Word, Word, Stability)

data Stability
  = StableRelease
  | PrereleaseBuild
  | NightlyBuild
  deriving stock (Eq, Ord, Show)

stabilityOf :: ListResult -> Stability
stabilityOf lr
  | Nightly `elem` lTag lr || LatestNightly `elem` lTag lr = NightlyBuild
  | Prerelease `elem` lTag lr || LatestPrerelease `elem` lTag lr = PrereleaseBuild
  | otherwise = StableRelease

-- | 'Nothing' when the first two version chunks are not both numeric. Such a
-- version is its own family and is never hidden by the latest-patch filter.
familyKey :: ListResult -> Maybe FamilyKey
familyKey lr = case _vChunks (lVer lr) of
  Chunks (Numeric major :| Numeric minor : _) -> Just (lCross lr, major, minor, stabilityOf lr)
  _ -> Nothing

-- | Newest version of each family. Rows with no family are absent, they are
-- handled by 'isLatestInFamily' directly.
latestPerFamily :: Vector ListResult -> Map FamilyKey Version
latestPerFamily = Vector.foldl' insertRow Map.empty
  where
    insertRow acc lr = case familyKey lr of
      Nothing -> acc
      Just key -> Map.insertWith max key (lVer lr) acc

isLatestInFamily :: Map FamilyKey Version -> ListResult -> Bool
isLatestInFamily newest lr = case familyKey lr of
  Nothing -> True
  Just key -> Map.lookup key newest == Just (lVer lr)
