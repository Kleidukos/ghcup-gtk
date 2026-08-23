module Toolchain.Curation
  ( curate
  ) where

import Data.Ord (Down (..))
import Data.Vector qualified as Vector
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..))

import Data.Functor
import Data.List qualified as List
import Toolchain.Types (Listings)

curate :: Bool -> Listings -> Listings
curate showOld listings =
  listings
    <&> Vector.filter keep
    <&> Vector.toList
    <&> List.sortOn (Down . lVer)
    <&> Vector.fromList
  where
    keep lr = installable lr && interesting lr
    installable lr = lInstalled lr || not (lNoBindist lr)
    interesting lr =
      showOld
        || Recommended `elem` lTag lr
        || Latest `elem` lTag lr
        || lInstalled lr
