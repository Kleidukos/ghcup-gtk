{-# OPTIONS_GHC -Wno-orphans #-}

module Presentation.Row
  ( Confirmation (..)
  , RowSpec (..)
  , RowAction (..)
  , ToolRows (..)
  , Pill (..)
  , installConfirmation
  , installVerb
  , jobTitle
  , matchesFilters
  , planRows
  , removeConfirmation
  , toolShortName
  ) where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Display
import Data.Time.Calendar (Day)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Versions (PVP, Version, prettyPVP, prettyVer)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..), TargetVersion, TargetVersionReq (..), Tool, cabal, ghc, ghcup, hls, stack, tVerToText)

import Config (Filters (..))
import Toolchain.Curation (FamilyKey, curate, isLatestInFamily, latestPerFamily)
import Toolchain.Types

data Confirmation = Confirmation
  { heading :: Text
  , body :: Text
  , affirmLabel :: Text
  , destructive :: Bool
  }
  deriving stock (Eq, Show)

data RowSpec = RowSpec
  { key :: RowKey
  , title :: Text
  , pills :: [Pill]
  , installed :: Bool
  , isDefault :: Bool
  , action :: RowAction
  , setDefault :: Mutation
  , tool :: Tool
  , installReq :: TargetVersionReq
  , rank :: Int
  , releaseDay :: Maybe Day
  , passesHlsFilter :: Bool
  , latestInFamily :: Bool
  , statusLabel :: Text
  , progress :: Maybe Progress
  -- ^ Set while a mutation is running on this row; renderers draw it as a
  -- pulsing bar plus the latest log line.
  }
  deriving stock (Eq, Show)

data Pill
  = HlsPowered
  | RecommendedVersion
  | LatestVersion
  deriving stock (Eq, Ord, Show)

instance Display Pill where
  displayBuilder HlsPowered = "hls-powered"
  displayBuilder RecommendedVersion = "recommended"
  displayBuilder LatestVersion = "latest"

data RowAction = RowAction
  { label :: Text
  , confirmation :: Confirmation
  , job :: Mutation
  }
  deriving stock (Eq, Show)

data ToolRows = ToolRows
  { rows :: Vector RowSpec
  , subtitle :: Text
  }
  deriving stock (Eq, Show)

planRows :: Map RowKey Progress -> Listings -> Map Tool ToolRows
planRows busy listings = Map.mapWithKey (planTool busy) (curate listings)

planTool :: Map RowKey Progress -> Tool -> Vector ListResult -> ToolRows
planTool busy tool toolRows =
  ToolRows
    { rows = Vector.imap (rowSpec busy tool newest) toolRows
    , subtitle = case lVer <$> Vector.find lSet toolRows of
        Just v -> "Default: " <> prettyVer v
        Nothing -> ""
    }
  where
    newest = latestPerFamily toolRows

rowSpec :: Map RowKey Progress -> Tool -> Map FamilyKey Version -> Int -> ListResult -> RowSpec
rowSpec busy tool newest rank lr =
  RowSpec
    { key
    , title
    , pills = mkListResultLabels lr
    , installed = lInstalled lr
    , isDefault = lSet lr
    , action =
        if lInstalled lr
          then RowAction "Remove" (removeConfirmation tool lr) (Uninstall tool (tvOf lr))
          else RowAction "Install" (installConfirmation tool lr) (Install tool (reqOf lr) defaultInstallOptions)
    , setDefault = SetDefault tool (tvOf lr)
    , tool
    , installReq = reqOf lr
    , rank
    , releaseDay = lReleaseDay lr
    , passesHlsFilter = passesHlsFilter tool lr
    , latestInFamily = isLatestInFamily newest lr
    , statusLabel = statusLabelOf lr
    , progress = Map.lookup key busy
    }
  where
    key = keyOfListing tool lr
    basePVP =
      case getBaseVersion lr.lTag of
        Nothing -> ""
        Just pvp -> " / base-" <> prettyPVP pvp
    title
      | tool == ghc = prettyVer lr.lVer <> basePVP
      | otherwise = prettyVer (lVer lr)

getBaseVersion :: [Tag] -> Maybe PVP
getBaseVersion tags = List.foldl' go Nothing tags
  where
    go :: Maybe PVP -> Tag -> Maybe PVP
    go Nothing (Base b) = Just b
    go Nothing _ = Nothing
    go (Just b) _ = Just b

passesHlsFilter :: Tool -> ListResult -> Bool
passesHlsFilter tool lr = tool /= ghc || hlsPowered lr

-- | Whether a row survives a filter bar's active filters. Shared by the list
-- and table renderers.
matchesFilters :: Filters -> RowSpec -> Bool
matchesFilters filters spec =
  (not filters.hlsPoweredOnly || spec.passesHlsFilter)
    && (not filters.latestPatchOnly || spec.latestInFamily)

statusLabelOf :: ListResult -> Text
statusLabelOf lr
  | lSet lr = "default"
  | lInstalled lr = "installed"
  | otherwise = ""

mkListResultLabels :: ListResult -> [Pill]
mkListResultLabels lr =
  let tagLabels = mapMaybe mkTagLabel lr.lTag
      hlsPoweredLabel = [HlsPowered | lr.hlsPowered]
  in tagLabels <> hlsPoweredLabel

mkTagLabel :: Tag -> Maybe Pill
mkTagLabel = \case
  Recommended -> Just RecommendedVersion
  Latest -> Just LatestVersion
  _ -> Nothing

subject :: Tool -> ListResult -> Text
subject tool lr = toolShortName tool <> " " <> prettyVer (lVer lr)

installConfirmation :: Tool -> ListResult -> Confirmation
installConfirmation tool lr =
  Confirmation
    { heading = "Install " <> subject tool lr <> "?"
    , body = "The download may take several minutes."
    , affirmLabel = "Install"
    , destructive = False
    }

jobTitle :: Mutation -> Text
jobTitle = \case
  Install tool (TargetVersionReq tv _) _ -> done tool tv "installed"
  Uninstall tool tv -> done tool tv "uninstalled"
  SetDefault tool tv -> done tool tv "is now the default"
  where
    done :: Tool -> TargetVersion -> Text -> Text
    done tool tv outcome = toolShortName tool <> " " <> tVerToText tv <> " " <> outcome

removeConfirmation :: Tool -> ListResult -> Confirmation
removeConfirmation tool lr =
  Confirmation
    { heading = "Uninstall " <> subject tool lr <> "?"
    , body = "The files will be removed from your system."
    , affirmLabel = "Uninstall"
    , destructive = True
    }

instance Display Tool where
  displayBuilder tool
    | tool == ghc = "Glasgow Haskell Compiler"
    | tool == cabal = "Cabal project manager"
    | tool == hls = "Haskell Language Server"
    | tool == stack = "Stack"
    | tool == ghcup = "GHCup"
    | otherwise = displayBuilder $ toolText tool

installVerb :: RowSpec -> Text
installVerb spec = if spec.installed then "Reinstall" else "Install"

-- | Short name for dialog headings and toasts.
toolShortName :: Tool -> Text
toolShortName tool
  | tool == ghc = "GHC"
  | tool == cabal = "Cabal"
  | tool == hls = "HLS"
  | tool == stack = "Stack"
  | tool == ghcup = "GHCup"
  | otherwise = toolText tool
