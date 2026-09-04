module Presentation.Row
  ( Confirmation (..)
  , RowSpec (..)
  , RowAction (..)
  , ToolRows (..)
  , installConfirmation
  , jobTitle
  , planRows
  , removeConfirmation
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Versions (Version, prettyVer)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..), TargetVersion, TargetVersionReq (..), tVerToText)

import Toolchain.Curation (CurationMode (..), FamilyKey, curate, isLatestInFamily, latestPerFamily)
import Toolchain.Types

data Confirmation = Confirmation
  { heading :: Text
  , body :: Text
  , affirmLabel :: Text
  , destructive :: Bool
  }
  deriving stock (Eq, Show)

-- | Everything a rendered version row shows and can do, as data: the
-- widget layer only draws labels and routes clicks.
data RowSpec = RowSpec
  { key :: RowKey
  , title :: Text
  , pills :: [Text]
  , installed :: Bool
  , isDefault :: Bool
  , action :: RowAction
  , setDefault :: Mutation
  , rank :: Int
  , releaseDay :: Maybe Day
  , passesHlsFilter :: Bool
  , latestInFamily :: Bool
  , statusLabel :: Text
  }
  deriving stock (Eq, Show)

data RowAction = RowAction
  { label :: Text
  , confirmation :: Confirmation
  , job :: Mutation
  }
  deriving stock (Eq, Show)

-- | One tool's pane: its rows (curated, sorted) and its sidebar subtitle.
data ToolRows = ToolRows
  { rows :: Vector RowSpec
  , subtitle :: Text
  }
  deriving stock (Eq, Show)

-- | The full row plan. Total over 'supportedTools': a tool with nothing to
-- show still gets an entry (empty rows, blank subtitle), so a rebuild
-- clears its pane and subtitle.
planRows :: CurationMode -> Listings -> Map SupportedTool ToolRows
planRows mode listings =
  let curated = curate mode listings
  in Map.fromList
       [ (tool, planTool tool (Map.findWithDefault Vector.empty tool curated))
       | tool <- Vector.toList supportedTools
       ]

planTool :: SupportedTool -> Vector ListResult -> ToolRows
planTool tool toolRows =
  ToolRows
    { rows = Vector.imap (rowSpec tool newest) toolRows
    , subtitle = case lVer <$> Vector.find lSet toolRows of
        Just v -> "Default: " <> prettyVer v
        Nothing -> ""
    }
  where
    newest = latestPerFamily toolRows

rowSpec :: SupportedTool -> Map FamilyKey Version -> Int -> ListResult -> RowSpec
rowSpec tool newest rank lr =
  RowSpec
    { key = keyOfListing tool lr
    , title = prettyVer (lVer lr)
    , pills = mapMaybe pillLabel (lTag lr)
    , installed = lInstalled lr
    , isDefault = lSet lr
    , action =
        if lInstalled lr
          then RowAction "Remove" (removeConfirmation tool lr) (Uninstall tool (tvOf lr))
          else RowAction "Install" (installConfirmation tool lr) (Install tool (reqOf lr))
    , setDefault = SetDefault tool (tvOf lr)
    , rank
    , releaseDay = lReleaseDay lr
    , passesHlsFilter = passesHlsFilter tool lr
    , latestInFamily = isLatestInFamily newest lr
    , statusLabel = statusLabelOf lr
    }

-- | The HLS-powered notion only exists for GHC releases, so the filter must
-- never hide another tool's rows: every non-GHC row passes.
passesHlsFilter :: SupportedTool -> ListResult -> Bool
passesHlsFilter tool lr = tool /= GHC || hlsPowered lr

statusLabelOf :: ListResult -> Text
statusLabelOf lr
  | lSet lr = "default"
  | lInstalled lr = "installed"
  | otherwise = ""

pillLabel :: Tag -> Maybe Text
pillLabel = \case
  Recommended -> Just "recommended"
  Latest -> Just "latest"
  _ -> Nothing

subject :: SupportedTool -> ListResult -> Text
subject tool lr = toolName tool <> " " <> prettyVer (lVer lr)

installConfirmation :: SupportedTool -> ListResult -> Confirmation
installConfirmation tool lr =
  Confirmation
    { heading = "Install " <> subject tool lr <> "?"
    , body = "The download may take several minutes."
    , affirmLabel = "Install"
    , destructive = False
    }

-- | Toast copy for a finished mutation.
jobTitle :: Mutation -> Text
jobTitle = \case
  Install tool (TargetVersionReq tv _) -> done tool tv "installed"
  Uninstall tool tv -> done tool tv "uninstalled"
  SetDefault tool tv -> done tool tv "is now the default"
  where
    done :: SupportedTool -> TargetVersion -> Text -> Text
    done tool tv outcome = toolName tool <> " " <> tVerToText tv <> " " <> outcome

removeConfirmation :: SupportedTool -> ListResult -> Confirmation
removeConfirmation tool lr =
  Confirmation
    { heading = "Uninstall " <> subject tool lr <> "?"
    , body = "The files will be removed from your system."
    , affirmLabel = "Uninstall"
    , destructive = True
    }
