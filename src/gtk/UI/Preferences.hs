module UI.Preferences
  ( ChannelsState (..)
  , present
  ) where

import Control.Monad (void)
import Data.GI.Base
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GI.Adw qualified as Adw
import URI.ByteString (URI)

import Session (ChannelsEditability (..))
import Toolchain.Channels (Channel (..), defaultNightliesUrl, nightliesMarker, parseNightlies, uriText)

-- | What the preferences dialog needs to render the channel toggles.
data ChannelsState = ChannelsState
  { channels :: Set Channel
  , nightliesUrl :: Maybe URI
  , editable :: ChannelsEditability
  }
  deriving stock (Eq, Show)

present
  :: Adw.ApplicationWindow
  -> ChannelsState
  -> (Set Channel -> Maybe URI -> IO ())
  -> IO ()
present parent channelsState onChannels = do
  channelsGroup <- buildChannelsGroup channelsState onChannels

  page <- new Adw.PreferencesPage []
  page.add channelsGroup

  dialog <- new Adw.PreferencesDialog []
  dialog.add page
  dialog.present (Just parent)

buildChannelsGroup :: ChannelsState -> (Set Channel -> Maybe URI -> IO ()) -> IO Adw.PreferencesGroup
buildChannelsGroup channelsState onChannels = do
  prereleasesToggle <-
    new
      Adw.SwitchRow
      [ #title := "Prereleases"
      , #subtitle := "Alpha, beta and release-candidate builds"
      , #active := Set.member Prereleases channelsState.channels
      ]
  crossToggle <-
    new
      Adw.SwitchRow
      [ #title := "Cross builds"
      , #subtitle := "GHC cross-compilation targets"
      , #active := Set.member Cross channelsState.channels
      ]
  thirdPartyToggle <-
    new
      Adw.SwitchRow
      [ #title := "Third-party tools"
      , #subtitle := "hlint, stan and other community tools"
      , #active := Set.member ThirdParty channelsState.channels
      ]
  nightliesEntry <-
    new
      Adw.EntryRow
      [ #title := "Metadata URL (file name must start with \"" <> nightliesMarker <> "\")"
      , #text := maybe defaultNightliesUrl uriText channelsState.nightliesUrl
      , #showApplyButton := True
      ]
  nightliesRow <-
    new
      Adw.ExpanderRow
      [ #title := "Nightlies"
      , #subtitle := "Nightly GHC builds; needs a metadata URL"
      , #showEnableSwitch := True
      , #enableExpansion := Set.member Nightlies channelsState.channels
      ]
  nightliesRow.addRow nightliesEntry

  let emitChannels = do
        prereleases <- prereleasesToggle.getActive
        cross <- crossToggle.getActive
        thirdParty <- thirdPartyToggle.getActive
        nightliesOn <- get nightliesRow #enableExpansion
        urlText <- nightliesEntry.getText
        let nightlies = parseNightlies urlText
            channels =
              Set.fromList $
                concat
                  [ [Prereleases | prereleases]
                  , [Cross | cross]
                  , [ThirdParty | thirdParty]
                  , [Nightlies | nightliesOn && isJust nightlies]
                  ]
        onChannels channels nightlies

      markValidity = do
        urlText <- nightliesEntry.getText
        if Text.null (Text.strip urlText) || isJust (parseNightlies urlText)
          then nightliesEntry.removeCssClass "error"
          else nightliesEntry.addCssClass "error"

  void $ on prereleasesToggle (PropertyNotify #active) $ const emitChannels
  void $ on crossToggle (PropertyNotify #active) $ const emitChannels
  void $ on thirdPartyToggle (PropertyNotify #active) $ const emitChannels
  void $ on nightliesRow (PropertyNotify #enableExpansion) $ const emitChannels
  void $ on nightliesEntry #apply emitChannels
  void $ on nightliesEntry #changed markValidity

  group <-
    new
      Adw.PreferencesGroup
      [ #title := "Channels"
      , #description := channelsDescription channelsState.editable
      , #sensitive := (channelsState.editable == ChannelsEditable)
      ]
  group.add prereleasesToggle
  group.add crossToggle
  group.add thirdPartyToggle
  group.add nightliesRow
  pure group

channelsDescription :: ChannelsEditability -> Text
channelsDescription = \case
  ChannelsEditable -> "Release channels, shared with the ghcup command line"
  ChannelsLocked -> "The ghcup configuration could not be read; channel editing is disabled"
