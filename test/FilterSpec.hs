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
    [ testCase "every tool gets the curation filters" $
        filtersFor @?= [ShowOldPatches]
    , testCase "GHC exposes every configured channel" $
        channelsFor allChannels ghc @?= [Prereleases, Nightlies, Cross]
    , testCase "unconfigured channels are hidden" $ do
        channelsFor (Set.singleton Cross) ghc @?= [Cross]
        channelsFor Set.empty ghc @?= []
    , testCase "every other tool only exposes prereleases" $ do
        channelsFor allChannels cabal @?= [Prereleases]
        channelsFor allChannels (Tool "hlint") @?= [Prereleases]
        channelsFor (Set.singleton Cross) cabal @?= []
    , testCase "third-party is never exposed as a per-tool filter" $ do
        channelsFor (Set.singleton ThirdParty) ghc @?= []
        channelsFor (Set.singleton ThirdParty) cabal @?= []
    , testCase "reachableChannels unions what the panes' filter bars can offer" $ do
        reachableChannels allChannels [ghc, cabal] @?= Set.fromList [Prereleases, Nightlies, Cross]
        reachableChannels allChannels [cabal] @?= Set.singleton Prereleases
        reachableChannels (Set.singleton ThirdParty) [ghc, cabal] @?= Set.empty
        reachableChannels allChannels ([] :: [Tool]) @?= Set.empty
    , testCase "filter and channel labels are stable" $ do
        filterLabel ShowOldPatches @?= "Show older patch releases"
        channelLabel Nightlies @?= "Nightlies"
        channelLabel ThirdParty @?= "Third-party tools"
    , testCase "activeCount counts kinds and channels together" $
        activeCount (ActiveFilters (Set.singleton ShowOldPatches) (Set.fromList [Cross, Nightlies])) @?= 3
    , testGroup
        "restrictTo"
        [ testCase "selections for a channel or kind no longer offered are dropped" $
            restrictTo [] [Prereleases] everything
              @?= ActiveFilters Set.empty (Set.singleton Prereleases)
        , testCase "surviving selections are kept" $
            restrictTo [ShowOldPatches] [Prereleases, Nightlies, Cross] everything @?= everything
        , testCase "an empty bar drops everything" $
            restrictTo [] [] everything @?= mempty
        ]
    ]
  where
    allChannels = Set.fromList [minBound .. maxBound]
    everything =
      ActiveFilters
        (Set.singleton ShowOldPatches)
        (Set.fromList [Prereleases, Nightlies, Cross])
