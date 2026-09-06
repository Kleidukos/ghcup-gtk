module Presentation.Path
  ( appliedBanner
  , pathBanner
  , pathFixConfirmation
  ) where

import Data.Function
import Data.Functor
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Presentation.Banner
import Presentation.Row (Confirmation (..))
import Toolchain.Path (FileChange (..), PathStatus (..), WriteMode (..), sourceLine)
import Toolchain.Types (GhcupDirs)

pathFixConfirmation :: Vector FileChange -> Confirmation
pathFixConfirmation changes =
  let filteredChanges =
        changes
          & Vector.filter (\c -> c.mode == FilteredAppend)
          <&> (.payload)
          & Vector.toList
      describeChange c =
        Text.pack c.path
          <> (if c.mode == CreateOrReplace then " (created, PATH setup)" else "")
  in Confirmation
       { heading = "Set Up Your PATH?"
       , body =
           Text.unlines $
             concat
               [ ["This will modify:", ""]
               , Vector.toList (Vector.map describeChange changes)
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
        , actions = []
        }
  NeedsFixManual ->
    Just
      BannerSpec
        { title = notFoundTitle
        , actions =
            [ ShowInfo
                InstructionsSpec
                  { buttonLabel = "How to fix…"
                  , dialogHeading = "Add ghcup to your PATH"
                  , dialogBody =
                      "Add this line to your shell configuration file:\n\n" <> sourceLine dirs
                  }
            ]
        }
  NeedsFixPlanned changes ->
    Just
      BannerSpec
        { title = notFoundTitle
        , actions =
            [ ConfirmFix
                OfferFixSpec
                  { buttonLabel = "Fix…"
                  , confirmation = pathFixConfirmation changes
                  }
            ]
        }
  where
    notFoundTitle = "Installed tools won't be found in your terminal"

appliedBanner :: BannerSpec
appliedBanner = BannerSpec {title = "Done – restart your terminal", actions = []}
