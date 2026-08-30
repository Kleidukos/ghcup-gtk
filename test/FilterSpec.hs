module FilterSpec (tests) where

import Data.Set qualified as Set
import GHCup.Types (Tool (..), cabal, ghc)
import Test.Tasty
import Test.Tasty.HUnit

import Presentation.Filter

tests :: TestTree
tests =
  testGroup
    "Filter"
    [ testCase "the simple GHC bar hides prerelease, nightly and cross filters" $
        filtersFor ghc @?= [HlsPoweredOnly, LatestPatchOnly]
    , testCase "the advanced GHC bar shows every filter" $
        advancedFiltersFor ghc @?= [minBound .. maxBound]
    , testCase "every other tool gets the generic bar in both views" $ do
        filtersFor cabal @?= [LatestPatchOnly, ShowPrereleases]
        filtersFor (Tool "hlint") @?= [LatestPatchOnly, ShowPrereleases]
        advancedFiltersFor cabal @?= [LatestPatchOnly, ShowPrereleases]
    , testCase "defaults: GHC curated, others latest-patch only" $ do
        defaultFilters ghc @?= Set.fromList [HlsPoweredOnly, LatestPatchOnly]
        defaultFilters cabal @?= Set.singleton LatestPatchOnly
    ]
