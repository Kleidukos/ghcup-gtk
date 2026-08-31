module Toolchain.ChannelsSpec (tests) where

import Data.Set qualified as Set
import GHCup.Hardcoded.URLs (channelURL)
import GHCup.Types (ChannelAlias (..), NewURLSource (..))
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString (URI)

import Fixtures (defaultNightliesUri, uriOf)
import Toolchain.Channels

nightlies :: URI
nightlies = defaultNightliesUri

custom :: URI
custom = uriOf "https://example.com/my-metadata.yaml"

legacyPrereleases :: URI
legacyPrereleases = uriOf "https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.7.yaml"

thirdParty :: URI
thirdParty = uriOf "https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-3rdparty-0.1.0.yaml"

tests :: TestTree
tests =
  testGroup
    "Toolchain.Channels"
    [ testCase "enabling channels appends them after existing entries" $
        applyChannels (Set.fromList [Prereleases, Cross]) Nothing [NewGHCupURL]
          @?= [NewGHCupURL, NewChannelAlias PrereleasesChannel, NewChannelAlias CrossChannel]
    , testCase "disabling removes recognised entries only" $
        applyChannels Set.empty Nothing [NewGHCupURL, NewChannelAlias PrereleasesChannel, NewURI custom]
          @?= [NewGHCupURL, NewURI custom]
    , testCase "already-enabled channels keep their position" $
        applyChannels (Set.singleton Prereleases) Nothing [NewChannelAlias PrereleasesChannel, NewGHCupURL]
          @?= [NewChannelAlias PrereleasesChannel, NewGHCupURL]
    , testCase "nightlies enabled uses the supplied URI" $
        applyChannels (Set.singleton Nightlies) (Just nightlies) [NewGHCupURL]
          @?= [NewGHCupURL, NewURI nightlies]
    , testCase "an existing nightlies entry is replaced in place by the supplied URI" $ do
        let updated = uriOf "https://ghc.gitlab.haskell.org/ghcup-metadata/ghcup-nightlies-0.0.8.yaml"
        applyChannels (Set.singleton Nightlies) (Just updated) [NewURI nightlies, NewGHCupURL]
          @?= [NewURI updated, NewGHCupURL]
    , testCase "an existing nightlies entry survives a Nothing URI" $
        applyChannels (Set.singleton Nightlies) Nothing [NewURI nightlies, NewGHCupURL]
          @?= [NewURI nightlies, NewGHCupURL]
    , testCase "an existing nightlies entry is not duplicated when the same URI is supplied again" $
        applyChannels (Set.singleton Nightlies) (Just nightlies) [NewURI nightlies, NewGHCupURL]
          @?= [NewURI nightlies, NewGHCupURL]
    , testCase "nightlies requested without a URI is skipped" $
        applyChannels (Set.singleton Nightlies) Nothing [NewGHCupURL]
          @?= [NewGHCupURL]
    , testCase "an emptied list falls back to the default source" $
        applyChannels Set.empty Nothing [NewChannelAlias PrereleasesChannel]
          @?= [NewGHCupURL]
    , testCase "applyChannels is idempotent" $ do
        let set = Set.fromList [Prereleases, Nightlies]
            once = applyChannels set (Just nightlies) [NewGHCupURL, NewURI custom]
        applyChannels set (Just nightlies) once @?= once
    , testCase "configuredChannels of the result is the requested set" $ do
        let sets =
              [ Set.empty
              , Set.singleton Cross
              , Set.fromList [Prereleases, Cross]
              , Set.fromList [Prereleases, Nightlies, Cross]
              , Set.fromList [minBound .. maxBound]
              ]
            inputs =
              [ [NewGHCupURL]
              , [NewGHCupURL, NewChannelAlias CrossChannel, NewURI custom]
              , [NewURI (channelURL PrereleasesChannel)]
              ]
        sequence_
          [ configuredChannels (applyChannels set (Just nightlies) input) @?= set
          | set <- sets
          , input <- inputs
          ]
    , testCase "enable then disable round-trips" $ do
        let original = [NewGHCupURL, NewURI custom]
            enabled = applyChannels (Set.fromList [Prereleases, Cross]) Nothing original
        applyChannels Set.empty Nothing enabled @?= original
    , testCase "a nightlies URI without the marker is refused" $
        applyChannels (Set.singleton Nightlies) (Just custom) [NewGHCupURL]
          @?= [NewGHCupURL]
    , testCase "only the first entry recognised as a channel survives" $
        applyChannels (Set.singleton Prereleases) Nothing [NewURI legacyPrereleases, NewChannelAlias PrereleasesChannel]
          @?= [NewURI legacyPrereleases]
    , testCase "nightliesUri finds the nightlies entry" $ do
        nightliesUri [NewGHCupURL, NewURI nightlies] @?= Just nightlies
        nightliesUri [NewGHCupURL, NewURI custom] @?= Nothing
        nightliesUri [NewChannelAlias PrereleasesChannel] @?= Nothing
    , testCase "the third-party channel alias is recognised" $
        sourceChannel (NewChannelAlias ThirdPartyChannel) @?= Just ThirdParty
    , testCase "the third-party metadata URI is recognised by its path marker" $
        sourceChannel (NewURI thirdParty) @?= Just ThirdParty
    , testCase "third-party enable then disable round-trips" $ do
        let original = [NewGHCupURL, NewURI custom]
            enabled = applyChannels (Set.singleton ThirdParty) Nothing original
        enabled @?= [NewGHCupURL, NewURI custom, NewChannelAlias ThirdPartyChannel]
        applyChannels Set.empty Nothing enabled @?= original
    , testGroup
        "marker matching"
        [ testCase "a versioned official file name is recognised" $
            sourceChannel (NewURI (uriOf "https://example.com/meta/ghcup-cross-0.1.0.yaml"))
              @?= Just Cross
        , testCase "a versioned nightlies file name is recognised" $
            sourceChannel (NewURI (uriOf "https://example.com/meta/ghcup-nightlies-0.0.7.yaml"))
              @?= Just Nightlies
        , testCase "an unversioned file name is recognised" $
            sourceChannel (NewURI (uriOf "https://example.com/meta/ghcup-nightlies.yaml"))
              @?= Just Nightlies
        , testCase "a longer name that merely starts with the marker is not a channel" $
            sourceChannel (NewURI (uriOf "https://example.com/meta/ghcup-cross-experiments.yaml"))
              @?= Nothing
        , testCase "a name that merely contains the marker is not a channel" $
            sourceChannel (NewURI (uriOf "https://example.com/meta/my-ghcup-cross-0.1.0.yaml"))
              @?= Nothing
        , testCase "a marker in a parent directory is not a channel" $
            sourceChannel (NewURI (uriOf "https://example.com/ghcup-cross-0.1.0/meta.yaml"))
              @?= Nothing
        , testCase "a user's own file is not deleted when its channel is disabled" $ do
            let ownFile = NewURI (uriOf "https://example.com/meta/ghcup-cross-experiments.yaml")
            applyChannels Set.empty Nothing [NewGHCupURL, ownFile]
              @?= [NewGHCupURL, ownFile]
        ]
    , testCase "the default nightlies URL parses as a nightlies URI" $ do
        parseNightlies defaultNightliesUrl @?= Just nightlies
        parseNightlies " not a uri " @?= Nothing
        parseNightlies "https://example.com/my-metadata.yaml" @?= Nothing
    , testCase "uriText round-trips a parsed URL" $
        uriText nightlies @?= defaultNightliesUrl
    ]
