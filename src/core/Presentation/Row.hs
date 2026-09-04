{-# OPTIONS_GHC -Wno-orphans #-}

module Presentation.Row
  ( Confirmation (..)
  , RowSpec (..)
  , RowAction (..)
  , ToolRows (..)
  , Pill (..)
  , compileGhcMutation
  , compileHlsMutation
  , defaultAction
  , installConfirmation
  , installMutation
  , installVerb
  , jobTitle
  , matchesFilters
  , planRows
  , removeConfirmation
  , setDefaultMutation
  , statusLabel
  , toolShortName
  ) where

import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Display
import Data.Time.Calendar (Day)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Versions (PVP, Version, prettyPVP, prettyVer)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..), TargetVersion, TargetVersionReq (..), Tool, cabal, ghc, ghcup, hls, stack, tVerToText)

import Presentation.Filter (FilterKind (..))
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
  , tool :: Tool
  , installReq :: TargetVersionReq
  , rank :: Int
  , releaseDay :: Maybe Day
  , latestInFamily :: Bool
  , progress :: Maybe Progress
  -- ^ Set while a mutation is running on this row; renderers draw it as a
  -- pulsing bar plus the latest log line.
  , isPrerelease :: Bool
  , isNightly :: Bool
  , crossTarget :: Maybe Text
  }
  deriving stock (Eq, Show)

data Pill
  = HlsPowered
  | RecommendedVersion
  | LatestVersion
  | PrereleaseVersion
  | NightlyVersion
  deriving stock (Eq, Ord, Show)

instance Display Pill where
  displayBuilder HlsPowered = "hls-powered"
  displayBuilder RecommendedVersion = "recommended"
  displayBuilder LatestVersion = "latest"
  displayBuilder PrereleaseVersion = "prerelease"
  displayBuilder NightlyVersion = "nightly"

data RowAction = RowAction
  { label :: Text
  , confirmation :: Confirmation
  , job :: Mutation
  }
  deriving stock (Eq, Show)

data ToolRows = ToolRows
  { rows :: Vector RowSpec
  , subtitle :: Text
  , installedGhcs :: [Version]
  }
  deriving stock (Eq, Show)

planRows :: Map RowKey Progress -> Listings -> Map Tool ToolRows
planRows busy listings = Map.mapWithKey (planTool busy installedGhcs) (curate listings)
  where
    installedGhcs =
      maybe
        []
        (\results -> Vector.filter lInstalled results & Vector.map lVer & Vector.toList)
        (Map.lookup ghc listings)

planTool :: Map RowKey Progress -> [Version] -> Tool -> Vector ListResult -> ToolRows
planTool busy installedGhcs tool toolRows =
  ToolRows
    { rows = Vector.imap (rowSpec busy tool newest) toolRows
    , subtitle = case lVer <$> Vector.find lSet toolRows of
        Just v -> "Default: " <> prettyVer v
        Nothing -> ""
    , installedGhcs = if canCompileFromSource tool then installedGhcs else []
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
    , tool
    , installReq = reqOf lr
    , rank
    , releaseDay = lReleaseDay lr
    , latestInFamily = isLatestInFamily newest lr
    , progress = Map.lookup key busy
    , isPrerelease = Prerelease `elem` lr.lTag || LatestPrerelease `elem` lr.lTag
    , isNightly = Nightly `elem` lr.lTag || LatestNightly `elem` lr.lTag
    , crossTarget = lr.lCross
    }
  where
    key = keyOfListing tool lr
    basePVP =
      case getBaseVersion lr.lTag of
        Nothing -> ""
        Just pvp -> " / base-" <> prettyPVP pvp
    title
      | tool == ghc = tVerToText (tvOf lr) <> basePVP
      | otherwise = tVerToText (tvOf lr)

getBaseVersion :: [Tag] -> Maybe PVP
getBaseVersion tags = List.foldl' go Nothing tags
  where
    go :: Maybe PVP -> Tag -> Maybe PVP
    go Nothing (Base b) = Just b
    go Nothing _ = Nothing
    go (Just b) _ = Just b

-- | Whether a row survives a bar's active filters. Every filter reveals a
-- category hidden by default. Shared by the list and table renderers.
matchesFilters :: Set FilterKind -> RowSpec -> Bool
matchesFilters active spec =
  and
    [ on ShowOldPatches || spec.latestInFamily
    , on ShowPrereleases || not spec.isPrerelease
    , on ShowNightlies || not spec.isNightly
    , on ShowCross || isNothing spec.crossTarget
    ]
  where
    on kind = Set.member kind active

statusLabel :: RowSpec -> Text
statusLabel spec
  | spec.isDefault = "default"
  | spec.installed = "installed"
  | otherwise = ""

specTv :: RowSpec -> TargetVersion
specTv spec = let TargetVersionReq tv _ = spec.installReq in tv

defaultAction :: RowSpec -> RowAction
defaultAction spec
  | spec.installed = RowAction "Remove" (removeConfirmation spec) (Uninstall spec.tool (specTv spec))
  | otherwise = RowAction "Install" (installConfirmation spec) (Install spec.tool spec.installReq defaultInstallOptions)

setDefaultMutation :: RowSpec -> Mutation
setDefaultMutation spec = SetDefault spec.tool (specTv spec)

installMutation :: RowSpec -> InstallOptions -> Mutation
installMutation spec = Install spec.tool spec.installReq

compileGhcMutation :: RowSpec -> CompileGhcOptions -> Mutation
compileGhcMutation spec = CompileGhc (specTv spec)

compileHlsMutation :: RowSpec -> CompileHlsOptions -> Mutation
compileHlsMutation spec = CompileHls (specTv spec)

mkListResultLabels :: ListResult -> [Pill]
mkListResultLabels lr =
  let tagLabels = mapMaybe mkTagLabel lr.lTag
      hlsPoweredLabel = [HlsPowered | lr.hlsPowered]
  in tagLabels <> hlsPoweredLabel

mkTagLabel :: Tag -> Maybe Pill
mkTagLabel = \case
  Recommended -> Just RecommendedVersion
  Latest -> Just LatestVersion
  Prerelease -> Just PrereleaseVersion
  LatestPrerelease -> Just PrereleaseVersion
  Nightly -> Just NightlyVersion
  LatestNightly -> Just NightlyVersion
  _ -> Nothing

subject :: RowSpec -> Text
subject spec = toolShortName spec.tool <> " " <> tVerToText (specTv spec)

installConfirmation :: RowSpec -> Confirmation
installConfirmation spec =
  Confirmation
    { heading = "Install " <> subject spec <> "?"
    , body = "The download may take several minutes."
    , affirmLabel = "Install"
    , destructive = False
    }

jobTitle :: Mutation -> Text
jobTitle = \case
  Install tool (TargetVersionReq tv _) _ -> done tool tv "installed"
  Uninstall tool tv -> done tool tv "uninstalled"
  SetDefault tool tv -> done tool tv "is now the default"
  CompileGhc tv _ -> done ghc tv "compiled and installed"
  CompileHls tv _ -> done hls tv "compiled and installed"
  where
    done :: Tool -> TargetVersion -> Text -> Text
    done tool tv outcome = toolShortName tool <> " " <> tVerToText tv <> " " <> outcome

removeConfirmation :: RowSpec -> Confirmation
removeConfirmation spec =
  Confirmation
    { heading = "Uninstall " <> subject spec <> "?"
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

-- | \"Install\" or \"Reinstall\"; the one place this verb is derived.
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
