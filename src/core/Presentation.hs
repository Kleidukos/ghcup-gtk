module Presentation
  ( Confirmation (..)
  , BannerSpec (..)
  , BannerAction (..)
  , InstructionsSpec (..)
  , OfferFixSpec (..)
  , Pill (..)
  , PillAccent (..)
  , RowSpec (..)
  , RowAction (..)
  , ToolRows (..)
  , installConfirmation
  , removeConfirmation
  , pathFixConfirmation
  , pathBanner
  , appliedBanner
  , planRows
  , jobTitle
  ) where

import Data.Function
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Versions (prettyVer)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..), TargetVersion, TargetVersionReq (..), tVerToText)

import Toolchain.Curation (curate)
import Toolchain.Path (FileChange (..), PathStatus (..), WriteMode (..), sourceLine)
import Toolchain.Types

data Confirmation = Confirmation
  { heading :: Text
  , body :: Text
  , affirmLabel :: Text
  , destructive :: Bool
  }
  deriving stock (Eq, Show)

data BannerSpec = BannerSpec
  { title :: Text
  , action :: Maybe BannerAction
  -- ^ 'Nothing' is a plain hint with no button.
  }
  deriving stock (Eq, Show)

data BannerAction
  = ShowInfo InstructionsSpec
  | ConfirmFix OfferFixSpec
  deriving stock (Eq, Show)

data InstructionsSpec = InstructionsSpec
  { buttonLabel :: Text
  , dialogHeading :: Text
  , dialogBody :: Text
  }
  deriving stock (Eq, Show)

data OfferFixSpec = OfferFixSpec
  { buttonLabel :: Text
  , confirmation :: Confirmation
  }
  deriving stock (Eq, Show)

-- | How much a pill should stand out. The widget layer picks the style
-- class; the meaning stays here.
data PillAccent
  = Neutral
  | Positive
  deriving stock (Eq, Show)

-- | A small badge shown at the end of a row.
data Pill = Pill
  { label :: Text
  , accent :: PillAccent
  }
  deriving stock (Eq, Show)

-- | Everything a rendered version row shows and can do, as data: the
-- widget layer only draws labels and routes clicks.
data RowSpec = RowSpec
  { key :: RowKey
  , title :: Text
  , pills :: [Pill]
  , installed :: Bool
  , isDefault :: Bool
  , action :: RowAction
  , setDefault :: Mutation
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
planRows :: Bool -> Listings -> Map SupportedTool ToolRows
planRows showOld listings =
  let curated = curate showOld listings
  in Map.fromList
       [ (tool, planTool tool (Map.findWithDefault Vector.empty tool curated))
       | tool <- Vector.toList supportedTools
       ]

planTool :: SupportedTool -> Vector ListResult -> ToolRows
planTool tool toolRows =
  ToolRows
    { rows = fmap (rowSpec tool) toolRows
    , subtitle = case lVer <$> Vector.find lSet toolRows of
        Just v -> "Default: " <> prettyVer v
        Nothing -> ""
    }

rowSpec :: SupportedTool -> ListResult -> RowSpec
rowSpec tool lr =
  RowSpec
    { key = keyOfListing tool lr
    , title = prettyVer (lVer lr)
    , pills =
        mapMaybe tagPill (lTag lr)
          <> [Pill "HLS-powered" Positive | tool == GHC, hlsPowered lr]
    , installed = lInstalled lr
    , isDefault = lSet lr
    , action =
        if lInstalled lr
          then RowAction "Remove" (removeConfirmation tool lr) (Uninstall tool (tvOf lr))
          else RowAction "Install" (installConfirmation tool lr) (Install tool (reqOf lr))
    , setDefault = SetDefault tool (tvOf lr)
    }

tagPill :: Tag -> Maybe Pill
tagPill = \case
  Recommended -> Just (Pill "recommended" Neutral)
  Latest -> Just (Pill "latest" Neutral)
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

pathFixConfirmation :: Vector FileChange -> Confirmation
pathFixConfirmation changes =
  let filteredChanges =
        changes
          & Vector.filter (\c -> c.mode == FilteredAppend)
          <&> (.payload)
          & Vector.toList
  in Confirmation
       { heading = "Set Up Your PATH?"
       , body =
           Text.unlines $
             concat
               [ ["This will modify:", ""]
               , [ Text.pack c.path
                     <> (if c.mode == CreateOrReplace then " (created, PATH setup)" else "")
                 | c <- Vector.toList changes
                 ]
               , ["", "Lines to be written:", ""]
               ]
               <> filteredChanges
       , affirmLabel = "Apply"
       , destructive = False
       }

pathBanner :: GhcupDirs -> PathStatus -> Maybe BannerSpec
pathBanner dirs = \case
  PathOk -> Nothing
  FixedAwaitingRestart ->
    Just
      BannerSpec
        { title = "Restart your terminal or session for the tools to be available"
        , action = Nothing
        }
  NeedsFixManual ->
    Just
      BannerSpec
        { title = notFoundTitle
        , action =
            Just $
              ShowInfo
                InstructionsSpec
                  { buttonLabel = "How to fix…"
                  , dialogHeading = "Add ghcup to your PATH"
                  , dialogBody =
                      "Add this line to your shell configuration file:\n\n" <> sourceLine dirs
                  }
        }
  NeedsFixPlanned changes ->
    Just
      BannerSpec
        { title = notFoundTitle
        , action =
            Just $
              ConfirmFix
                OfferFixSpec
                  { buttonLabel = "Fix…"
                  , confirmation = pathFixConfirmation changes
                  }
        }
  where
    notFoundTitle = "Installed tools won't be found in your terminal"

appliedBanner :: BannerSpec
appliedBanner = BannerSpec {title = "Done — restart your terminal", action = Nothing}
