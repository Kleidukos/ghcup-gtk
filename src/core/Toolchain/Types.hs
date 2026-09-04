module Toolchain.Types
  ( toolText
  , sortTools
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

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (TargetVersion (..), TargetVersionReq (..), Tool (..), tVerToText, toolPriority)
import Text.Read (readMaybe)

type Listings = Map Tool (Vector ListResult)

data GhcupDirs = GhcupDirs
  { ghcupBinDir :: FilePath
  , ghcupBaseDir :: FilePath
  }
  deriving stock (Eq, Show)

-- | A job that changes an installation and sends the UI a 'JobDone'.
data Mutation
  = Install Tool TargetVersionReq
  | Uninstall Tool TargetVersion
  | SetDefault Tool TargetVersion
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
      Install tool (TargetVersionReq tv _) -> (tool, tv)
      Uninstall tool tv -> (tool, tv)
      SetDefault tool tv -> (tool, tv)

-- | Stable text encoding of a 'RowKey', for widget models that can only hold
-- strings
rowKeyText :: RowKey -> Text
rowKeyText (RowKey (tool, ver)) = toolText tool <> ":" <> ver

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

-- | The tool's identifier as it appears in ghcup metadata, e.g. "ghc",
-- "hlint".
toolText :: Tool -> Text
toolText (Tool name) = Text.pack name

-- | Sidebar order: ghcup's priority ranking for the tools it knows
-- (ghc, cabal, hls, stack, ghcup), then everything else alphabetically.
sortTools :: [Tool] -> [Tool]
sortTools = List.sortOn (\tool -> (fromMaybe maxBound (toolPriority tool), toolText tool))
