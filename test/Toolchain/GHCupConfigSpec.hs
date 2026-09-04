module Toolchain.GHCupConfigSpec (tests) where

import Data.ByteString.Char8 qualified as BS8
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import GHCup.Types
import GHCup.Types.JSON ()
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString (URI)

import Fixtures (uriOf)
import Toolchain.Channels (Channel (..))
import Toolchain.GHCup (updateUrlSource)

external :: URI
external = uriOf "https://example.com/my-metadata.yaml"

-- | A config carrying settings the dialog must not touch and a url-source
-- entry 'updateUrlSource' must not recognise as a channel.
sample :: UserSettings
sample =
  either (error . show) id . Yaml.decodeEither' @UserSettings . BS8.unlines $
    [ "cache: true"
    , "no-verify: true"
    , "url-source:"
    , "  - GHCupURL"
    , "  - https://example.com/my-metadata.yaml"
    , "  - prereleases"
    ]

tests :: TestTree
tests =
  testGroup
    "ghcup config round-trip"
    [ testCase "updating url-source preserves other settings" $ do
        let updated = updateUrlSource (Set.singleton Prereleases) Nothing sample
        reparsed <- either (assertFailure . show) pure (Yaml.decodeEither' @UserSettings (Yaml.encode updated))
        uUrlSource reparsed
          @?= Just
            (SimpleList [NewGHCupURL, NewURI external, NewChannelAlias PrereleasesChannel])
        uCache reparsed @?= Just True
        uNoVerify reparsed @?= Just True
    , testCase "unrecognised entries pass through and channels are rewritten" $ do
        let updated = updateUrlSource (Set.fromList [Cross, ThirdParty]) Nothing sample
        uUrlSource updated
          @?= Just
            ( SimpleList
                [ NewGHCupURL
                , NewURI external
                , NewChannelAlias CrossChannel
                , NewChannelAlias ThirdPartyChannel
                ]
            )
    , testCase "disabling every channel keeps the user's own entries" $ do
        let updated = updateUrlSource Set.empty Nothing sample
        uUrlSource updated @?= Just (SimpleList [NewGHCupURL, NewURI external])
    , testCase "a rewrite survives an encode/decode round-trip unchanged" $ do
        let updated = updateUrlSource (Set.singleton Cross) Nothing sample
        reparsed <- either (assertFailure . show) pure (Yaml.decodeEither' @UserSettings (Yaml.encode updated))
        uUrlSource reparsed @?= uUrlSource updated
    ]
