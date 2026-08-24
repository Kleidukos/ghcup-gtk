module Toolchain.Types
  ( SupportedTool (..)
  , supportedTools
  , toGhcupTool
  , fromGhcupTool
  , toolName
  , Listings
  , GhcupDirs (..)
  , Mutation (..)
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

import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Text.Read (readMaybe)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (TargetVersion (..), TargetVersionReq (..), Tool, cabal, ghc, hls, stack, tVerToText)

data SupportedTool = GHC | Cabal | HLS | Stack
  deriving stock (Eq, Ord, Show, Enum, Bounded)

supportedTools :: Vector SupportedTool
supportedTools = Vector.fromList [minBound .. maxBound]

toGhcupTool :: SupportedTool -> Tool
toGhcupTool = \case
  GHC -> ghc
  Cabal -> cabal
  HLS -> hls
  Stack -> stack

fromGhcupTool :: Tool -> Maybe SupportedTool
fromGhcupTool tool = Vector.find (\supported -> toGhcupTool supported == tool) supportedTools

toolName :: SupportedTool -> Text
toolName = \case
  GHC -> "GHC"
  Cabal -> "Cabal"
  HLS -> "HLS"
  Stack -> "Stack"

type Listings = Map SupportedTool (Vector ListResult)

data GhcupDirs = GhcupDirs
  { ghcupBinDir :: FilePath
  , ghcupBaseDir :: FilePath
  }
  deriving stock (Eq, Show)

-- | A job that changes an installation and sends the UI a 'JobDone'.
data Mutation
  = Install SupportedTool TargetVersionReq
  | Uninstall SupportedTool TargetVersion
  | SetDefault SupportedTool TargetVersion
  deriving stock (Eq, Show)

data Job
  = RefreshListings
  | Mutate Mutation
  deriving stock (Eq, Show)

tvOf :: ListResult -> TargetVersion
tvOf lr = TargetVersion (lCross lr) (lVer lr)

reqOf :: ListResult -> TargetVersionReq
reqOf lr = TargetVersionReq (tvOf lr) (Just (fst (lRev lr)))

newtype RowKey = RowKey (SupportedTool, Text)
  deriving stock (Eq, Ord, Show)

keyOfListing :: SupportedTool -> ListResult -> RowKey
keyOfListing tool lr = RowKey (tool, tVerToText (tvOf lr))

keyOfMutation :: Mutation -> RowKey
keyOfMutation mutation =
  let (tool, tv) = target mutation
  in RowKey (tool, tVerToText tv)
  where
    target = \case
      Install tool (TargetVersionReq tv _) -> (tool, tv)
      Uninstall tool tv -> (tool, tv)
      SetDefault tool tv -> (tool, tv)

-- | Stable text encoding of a 'RowKey', for widget models that can only hold
-- strings
rowKeyText :: RowKey -> Text
rowKeyText (RowKey (tool, ver)) = Text.pack (show tool) <> ":" <> ver

data Progress = Progress
  { phase :: Text
  , fraction :: Maybe Double
  -- ^ Set when the log line carried a percentage, so renderers can show a
  -- determinate bar instead of pulsing
  }
  deriving stock (Eq, Show)

-- | Progress from a raw ghcup log line, reading a "NN%" token if one appears.
progressOf :: Text -> Progress
progressOf line = Progress (Text.strip line) (fractionOf line)

fractionOf :: Text -> Maybe Double
fractionOf line =
  listToMaybe
    [ percent / 100
    | word <- reverse (Text.words line)
    , Just number <- [Text.stripSuffix "%" word]
    , Just percent <- [readMaybe (Text.unpack number)]
    , percent >= 0
    , percent <= 100
    ]

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
