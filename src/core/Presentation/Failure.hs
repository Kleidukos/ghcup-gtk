module Presentation.Failure
  ( failureBanner
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import GHCup.Types (TargetVersion, TargetVersionReq (..), Tool, ghc, hls, tVerToText)

import Presentation.Banner
import Presentation.Row (toolShortName)
import Toolchain.Types (Mutation (..), OpError (..))

failureBanner :: Mutation -> OpError -> BannerSpec
failureBanner mutation err =
  BannerSpec
    { title = failedTitle mutation <> foldMap (\path -> " \nView logs at: " <> Text.pack path) err.logFile
    , actions = foldMap (\path -> [OpenLog path]) err.logFile <> [Dismiss]
    }

failedTitle :: Mutation -> Text
failedTitle = \case
  Install tool (TargetVersionReq tv _) _ -> subject tool tv <> " installation failed."
  Uninstall tool tv -> subject tool tv <> " uninstall failed."
  SetDefault tool tv -> subject tool tv <> " could not be set as default."
  CompileGhc tv _ -> subject ghc tv <> " compilation failed."
  CompileHls tv _ -> subject hls tv <> " compilation failed."
  where
    subject :: Tool -> TargetVersion -> Text
    subject tool tv = toolShortName tool <> " " <> tVerToText tv
