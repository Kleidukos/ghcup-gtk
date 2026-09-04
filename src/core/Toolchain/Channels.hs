module Toolchain.Channels
  ( Channel (..)
  , applyChannels
  , configuredChannels
  , defaultNightliesUrl
  , nightliesMarker
  , nightliesUri
  , parseNightlies
  , sourceChannel
  , uriText
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import GHCup.Types (ChannelAlias (..), NewURLSource (..))
import URI.ByteString (URI, parseURI, serializeURIRef', strictURIParserOptions, uriPath)

data Channel
  = Prereleases
  | Nightlies
  | Cross
  | ThirdParty
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | The metadata channel a filter maps onto; 'Nightlies' has no
-- 'ChannelAlias' upstream yet.
channelAlias :: Channel -> Maybe ChannelAlias
channelAlias = \case
  Prereleases -> Just PrereleasesChannel
  Nightlies -> Nothing
  Cross -> Just CrossChannel
  ThirdParty -> Just ThirdPartyChannel

-- | The channels a ghcup url-source configuration enables.
configuredChannels :: [NewURLSource] -> Set Channel
configuredChannels = Set.fromList . mapMaybe sourceChannel

-- | The marker a channel's metadata URI carries in its file name.
-- Recognition must look at the path alone, never the host, query or
-- fragment.
channelMarker :: Channel -> ByteString
channelMarker = \case
  Prereleases -> "ghcup-prereleases"
  Nightlies -> "ghcup-nightlies"
  Cross -> "ghcup-cross"
  ThirdParty -> "ghcup-3rdparty"

-- | Recognition must not claim a file the user named after a channel: the
-- last path segment has to /start/ with the marker and the marker has to be
-- followed by the extension or by a version, never by more name.
hasMarker :: Channel -> URI -> Bool
hasMarker channel uri =
  case Char8.stripPrefix (channelMarker channel) basename of
    Nothing -> False
    Just rest -> case Char8.uncons rest of
      Just ('.', _) -> True
      Just ('-', after) -> maybe False (isDigit . fst) (Char8.uncons after)
      _ -> False
  where
    basename = uriPath uri & Char8.breakEnd (== '/') & snd

-- | Whether a URI names a nightlies metadata file, the check the
-- preferences dialog and 'sourceChannel' must agree on.
hasNightliesMarker :: URI -> Bool
hasNightliesMarker = hasMarker Nightlies

-- | The nightlies marker, for UI copy that must agree with recognition.
nightliesMarker :: Text
nightliesMarker = Text.Encoding.decodeUtf8 (channelMarker Nightlies)

-- | The nightlies metadata URL offered as a starting point. The metadata
-- version is pinned upstream, so this has to be bumped whenever upstream
-- rolls it.
defaultNightliesUrl :: Text
defaultNightliesUrl = "https://ghc.gitlab.haskell.org/ghcup-metadata/ghcup-nightlies-0.0.7.yaml"

uriText :: URI -> Text
uriText uri =
  serializeURIRef' uri
    & Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode

-- | Parse a user-supplied nightlies metadata URL, rejecting anything that
-- is not a strictly valid URI carrying the nightlies marker.
parseNightlies :: Text -> Maybe URI
parseNightlies input =
  case parseURI strictURIParserOptions (Text.Encoding.encodeUtf8 (Text.strip input)) of
    Right uri | hasNightliesMarker uri -> Just uri
    _ -> Nothing

-- | The channel a ghcup url-source configuration enables. Channel URIs
-- are recognised by the marker in their path, so versioned and legacy
-- spellings of an official URL classify the same way.
sourceChannel :: NewURLSource -> Maybe Channel
sourceChannel = \case
  NewChannelAlias alias -> find (\channel -> channelAlias channel == Just alias) allChannels
  NewURI uri -> find (`hasMarker` uri) allChannels
  _ -> Nothing
  where
    allChannels = [minBound .. maxBound]

-- | Rewrite a url-source list so exactly the requested channels are
-- enabled. Entries 'sourceChannel' does not recognise pass through in
-- place; the first entry recognised as a channel keeps its position and
-- later duplicates of that channel are dropped; missing channels are
-- appended. Nightlies needs a URI carrying the \"ghcup-nightlies\" path
-- marker and is skipped without one, so
-- @configuredChannels . applyChannels requested@ is always @requested@.
applyChannels :: Set Channel -> Maybe URI -> [NewURLSource] -> [NewURLSource]
applyChannels requested nightlies sources =
  case kept <> appended of
    [] -> [NewGHCupURL]
    result -> result
  where
    validNightlies = nightlies >>= \uri -> if hasNightliesMarker uri then Just uri else Nothing
    kept = keep Set.empty sources
    keep _ [] = []
    keep seen (source : rest) = case sourceChannel source of
      Nothing -> source : keep seen rest
      Just channel
        | channel `Set.notMember` requested || channel `Set.member` seen -> keep seen rest
        | channel == Nightlies -> maybe source NewURI validNightlies : keep (Set.insert channel seen) rest
        | otherwise -> source : keep (Set.insert channel seen) rest
    present = Set.fromList (mapMaybe sourceChannel kept)
    appended =
      Set.toAscList (requested `Set.difference` present)
        & mapMaybe sourceFor
    sourceFor = \case
      Nightlies -> NewURI <$> validNightlies
      channel -> NewChannelAlias <$> channelAlias channel

-- | The URI of the nightlies entry, when one is configured.
nightliesUri :: [NewURLSource] -> Maybe URI
nightliesUri sources =
  listToMaybe [uri | source@(NewURI uri) <- sources, sourceChannel source == Just Nightlies]
