module FilterSpec (tests) where

import GHCup.Types (Tool (..), cabal, ghc)
import Test.Tasty
import Test.Tasty.HUnit

import Presentation.Filter

tests :: TestTree
tests =
  testGroup
    "Filter"
    [ testCase "the simple GHC bar hides prerelease, nightly and cross filters" $
        filtersFor ghc @?= [ShowOldPatches]
    , testCase "the advanced GHC bar adds the remaining filters" $
        extraFiltersFor ghc @?= [ShowPrereleases, ShowNightlies, ShowCross]
    , testCase "every other tool gets the generic bar in both views" $ do
        filtersFor cabal @?= [ShowOldPatches, ShowPrereleases]
        filtersFor (Tool "hlint") @?= [ShowOldPatches, ShowPrereleases]
        extraFiltersFor cabal @?= []
    , testCase "simple and advanced bars never overlap" $ do
        filter (`elem` filtersFor ghc) (extraFiltersFor ghc) @?= []
        filtersFor ghc <> extraFiltersFor ghc @?= [minBound .. maxBound]
    ]
