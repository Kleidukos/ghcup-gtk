module Toolchain.Types
  ( toolText
  , sortTools
  , isCoreTool
  , Listings
  , GhcupDirs (..)
  , Mutation (..)
  , InstallOptions (..)
  , CompileGhcOptions (..)
  , CompileHlsOptions (..)
  , canCompileFromSource
  , defaultInstallOptions
  , Job (..)
  , tvOf
  , reqOf
  , RowKey
  , keyOfListing
  , keyOfMutation
  , rowKeyText
  , Progress (..)
  , progressOf
  , OpError (..)
  , UiMsg (..)
  ) where

import Control.Monad (guard)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Versions (Version)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (BuildSystem, InstallDir (..), TargetVersion (..), TargetVersionReq (..), Tool (..), VersionPattern, ghc, hls, tVerToText, toolPriority)
import Text.Read (readMaybe)
import URI.ByteString (URI)

type Listings = Map Tool (Vector ListResult)

data GhcupDirs = GhcupDirs
  { ghcupBinDir :: FilePath
  , ghcupBaseDir :: FilePath
  }
  deriving stock (Eq, Show)

data InstallOptions = InstallOptions
  { setAsDefault :: Bool
  , forceInstall :: Bool
  , installDir :: InstallDir
  , bindistUrl :: Maybe URI
  , extraConfArgs :: [String]
  , installTargets :: Maybe [String]
  }
  deriving stock (Eq, Show)

defaultInstallOptions :: InstallOptions
defaultInstallOptions =
  InstallOptions
    { setAsDefault = False
    , forceInstall = False
    , installDir = GHCupInternal
    , bindistUrl = Nothing
    , extraConfArgs = []
    , installTargets = Nothing
    }

data CompileGhcOptions = CompileGhcOptions
  { bootstrapGhc :: Either Version FilePath
  , hadrianGhc :: Maybe (Either Version FilePath)
  , jobs :: Maybe Int
  , buildConfig :: Maybe FilePath
  , patches :: Maybe (Either FilePath [URI])
  , crossTarget :: Maybe Text
  , addConfArgs :: [String]
  , setCompile :: Bool
  , overwriteVer :: Maybe [VersionPattern]
  , buildFlavour :: Maybe String
  , buildSystem :: Maybe BuildSystem
  , isolateDir :: Maybe FilePath
  , gitRef :: Maybe String
  , installTargets :: Maybe [String]
  , docs :: Maybe String
  }
  deriving stock (Eq, Show)

data CompileHlsOptions = CompileHlsOptions
  { targetGhcs :: [Version]
  , jobs :: Maybe Int
  , setCompile :: Bool
  , updateCabal :: Bool
  , overwriteVer :: Maybe [VersionPattern]
  , isolateDir :: Maybe FilePath
  , cabalProject :: Maybe (Either FilePath URI)
  , cabalProjectLocal :: Maybe URI
  , patches :: Maybe (Either FilePath [URI])
  , cabalArgs :: [Text]
  , gitRef :: Maybe String
  }
  deriving stock (Eq, Show)

canCompileFromSource :: Tool -> Bool
canCompileFromSource tool = tool == ghc || tool == hls

-- | A job that changes an installation and sends the UI a 'JobDone'.
data Mutation
  = Install Tool TargetVersionReq InstallOptions
  | Uninstall Tool TargetVersion
  | SetDefault Tool TargetVersion
  | CompileGhc TargetVersion CompileGhcOptions
  | CompileHls TargetVersion CompileHlsOptions
  deriving stock (Eq, Show)

data Job
  = RefreshListings
  | Mutate Mutation
  deriving stock (Eq, Show)

tvOf :: ListResult -> TargetVersion
tvOf lr = TargetVersion (lCross lr) (lVer lr)

reqOf :: ListResult -> TargetVersionReq
reqOf lr = TargetVersionReq (tvOf lr) (Just (fst (lRev lr)))

newtype RowKey = RowKey (Tool, Text)
  deriving stock (Eq, Ord, Show)

keyOfListing :: Tool -> ListResult -> RowKey
keyOfListing tool lr = RowKey (tool, tVerToText (tvOf lr))

keyOfMutation :: Mutation -> RowKey
keyOfMutation mutation =
  let (tool, tv) = target mutation
  in RowKey (tool, tVerToText tv)
  where
    target = \case
      Install tool (TargetVersionReq tv _) _ -> (tool, tv)
      Uninstall tool tv -> (tool, tv)
      SetDefault tool tv -> (tool, tv)
      CompileGhc tv _ -> (ghc, tv)
      CompileHls tv _ -> (hls, tv)

rowKeyText :: RowKey -> Text
rowKeyText (RowKey (tool, ver)) = toolText tool <> ":" <> ver

data Progress = Progress
  { phase :: Text
  , fraction :: Maybe Double
  }
  deriving stock (Eq, Show)

-- | Progress from a raw ghcup log line, reading a "NN%" token if one appears.
progressOf :: Text -> Progress
progressOf line = Progress (Text.strip line) (fractionOf line)

fractionOf :: Text -> Maybe Double
fractionOf line =
  listToMaybe (reverse (mapMaybe percentOf (Text.words line)))
  where
    percentOf word = do
      number <- Text.stripSuffix "%" word
      percent <- readMaybe (Text.unpack number)
      guard (percent >= 0 && percent <= 100)
      pure (percent / 100)

data OpError = OpError
  { title :: Text
  , details :: Text
  }
  deriving stock (Eq, Show)

data UiMsg
  = ListingsReady Listings Bool
  | ListingsFailed OpError
  | JobProgress Job Progress
  | JobDone Mutation (Either OpError ())
  deriving stock (Eq, Show)

toolText :: Tool -> Text
toolText (Tool name) = Text.pack name

sortTools :: [Tool] -> [Tool]
sortTools = List.sortOn (\tool -> (fromMaybe maxBound (toolPriority tool), toolText tool))

isCoreTool :: Tool -> Bool
isCoreTool = isJust . toolPriority
