module Toolchain.Curation
  ( CurationMode (..)
  , FamilyKey
  , curate
  , familyKey
  , isLatestInFamily
  , latestPerFamily
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

-- | How we curate a version list
data CurationMode
  = Curated Bool
  | Full
  deriving stock (Eq, Show)

curate :: CurationMode -> Listings -> Listings
curate mode listings =
  listings
    <&> Vector.filter keep
    <&> Vector.toList
    <&> List.sortOn (Down . lVer)
    <&> Vector.fromList
  where
    keep lr = installable lr && interesting lr
    installable lr = lInstalled lr || not (lNoBindist lr)
    interesting lr = case mode of
      Full -> True
      Curated showOld ->
        showOld
          || Recommended `elem` lTag lr
          || Latest `elem` lTag lr
          || lInstalled lr

-- | A release family: everything sharing a cross-compilation target and a Major.Minor version.
type FamilyKey = (Maybe Text, Word, Word)

-- | 'Nothing' when the first two version chunks are not both numeric. Such a
-- version is its own family and is never hidden by the latest-patch filter.
familyKey :: ListResult -> Maybe FamilyKey
familyKey lr = case _vChunks (lVer lr) of
  Chunks (Numeric major :| Numeric minor : _) -> Just (lCross lr, major, minor)
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
