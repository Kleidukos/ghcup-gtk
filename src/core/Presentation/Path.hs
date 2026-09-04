module Presentation.Path
  ( BannerSpec (..)
  , BannerAction (..)
  , InstructionsSpec (..)
  , OfferFixSpec (..)
  , appliedBanner
  , pathBanner
  , pathFixConfirmation
  ) where

import Data.Function
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Presentation.Row (Confirmation (..))
import Toolchain.Path (FileChange (..), PathStatus (..), WriteMode (..), sourceLine)
import Toolchain.Types (GhcupDirs)

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
