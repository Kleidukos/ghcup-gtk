module FilterSpec (tests) where

import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHCup.Types (Tool (..), cabal, ghc)
import Test.Tasty
import Test.Tasty.HUnit

import Presentation.Filter

tests :: TestTree
tests =
  testGroup
    "Filter"
    [ testCase "filterName round-trips for every kind" $
        forM_ [minBound .. maxBound] $ \kind ->
          filterFromName (filterName kind) @?= Just kind
    , testCase "an unknown name parses to Nothing" $
        filterFromName "colour" @?= Nothing
    , testCase "the GHC bar shows every filter" $
        filtersFor ghc @?= [minBound .. maxBound]
    , testCase "every other tool gets the generic bar" $ do
        filtersFor cabal @?= [LatestPatchOnly, ShowPrereleases]
        filtersFor (Tool "hlint") @?= [LatestPatchOnly, ShowPrereleases]
    , testCase "defaults: GHC curated, others latest-patch only" $ do
        defaultFilters ghc @?= Set.fromList [HlsPoweredOnly, LatestPatchOnly]
        defaultFilters cabal @?= Set.singleton LatestPatchOnly
    , testCase "activeFilters falls back to the tool's defaults" $ do
        activeFilters ghc Map.empty @?= defaultFilters ghc
        activeFilters ghc (Map.singleton ghc (Set.singleton ShowNightlies))
          @?= Set.singleton ShowNightlies
    ]
