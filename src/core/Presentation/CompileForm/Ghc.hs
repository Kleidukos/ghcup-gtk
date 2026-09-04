module Presentation.CompileForm.Ghc
  ( GhcFormModel (..)
  , GhcFormEvent (..)
  , GhcField (..)
  , initGhcFormModel
  , stepGhcForm
  , ghcFieldError
  , canCompileGhc
  , toGhcOptions
  ) where

import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Either (isRight)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Versions (Version, prettyVer)
import GHCup.Input.Parsers (absolutePathParser)
import GHCup.Types (BuildSystem (..))

import Presentation.CompileForm
import Toolchain.Types (CompileGhcOptions)
import Toolchain.Types qualified as Types

data GhcFormModel = GhcFormModel
  { bootstrapGhc :: Text
  , hadrianGhc :: Text
  , jobs :: Text
  , buildConfig :: Text
  , patches :: Text
  , crossTarget :: Text
  , addConfArgs :: Text
  , setCompile :: Bool
  , priorSetCompile :: Bool
  , overwriteVer :: Text
  , buildFlavour :: Text
  , buildSystem :: Maybe BuildSystem
  , isolateDir :: Maybe FilePath
  , gitRef :: Text
  , installTargets :: Text
  , docs :: Text
  }
  deriving stock (Eq, Show)

data GhcFormEvent
  = GhcBootstrapChanged Text
  | GhcHadrianChanged Text
  | GhcJobsChanged Text
  | GhcBuildConfigChanged Text
  | GhcPatchesChanged Text
  | GhcCrossTargetChanged Text
  | GhcConfArgsChanged Text
  | GhcSetToggled Bool
  | GhcOverwriteChanged Text
  | GhcFlavourChanged Text
  | GhcBuildSystemChanged (Maybe BuildSystem)
  | GhcIsolatePicked FilePath
  | GhcIsolateCleared
  | GhcGitRefChanged Text
  | GhcInstallTargetsChanged Text
  | GhcDocsChanged Text
  deriving stock (Eq, Show)

initGhcFormModel :: [Version] -> GhcFormModel
initGhcFormModel installedGhcs =
  GhcFormModel
    { bootstrapGhc = case installedGhcs of
        [] -> ""
        versions -> prettyVer (maximum versions)
    , hadrianGhc = ""
    , jobs = ""
    , buildConfig = ""
    , patches = ""
    , crossTarget = ""
    , addConfArgs = ""
    , setCompile = False
    , priorSetCompile = False
    , overwriteVer = ""
    , buildFlavour = ""
    , buildSystem = Nothing
    , isolateDir = Nothing
    , gitRef = ""
    , installTargets = ""
    , docs = ""
    }

stepGhcForm :: GhcFormEvent -> GhcFormModel -> GhcFormModel
stepGhcForm event model = case event of
  GhcBootstrapChanged text -> model {bootstrapGhc = text}
  GhcHadrianChanged text -> model {hadrianGhc = text}
  GhcJobsChanged text -> model {jobs = text}
  GhcBuildConfigChanged text -> model {buildConfig = text}
  GhcPatchesChanged text -> model {patches = text}
  GhcCrossTargetChanged text -> model {crossTarget = text}
  GhcConfArgsChanged text -> model {addConfArgs = text}
  GhcSetToggled b
    | isJust model.isolateDir -> model
    | otherwise -> model {setCompile = b, priorSetCompile = b}
  GhcOverwriteChanged text -> model {overwriteVer = text}
  GhcFlavourChanged text -> model {buildFlavour = text}
  GhcBuildSystemChanged system -> model {buildSystem = system}
  GhcIsolatePicked path
    | isJust model.isolateDir -> model {isolateDir = Just path}
    | otherwise -> model {isolateDir = Just path, setCompile = False}
  GhcIsolateCleared -> model {isolateDir = Nothing, setCompile = model.priorSetCompile}
  GhcGitRefChanged text -> model {gitRef = text}
  GhcInstallTargetsChanged text -> model {installTargets = text}
  GhcDocsChanged text -> model {docs = text}

data GhcField
  = GhcBootstrapField
  | GhcHadrianField
  | GhcJobsField
  | GhcBuildConfigField
  | GhcPatchesField
  | GhcOverwriteField
  deriving stock (Eq, Show)

ghcFieldError :: GhcFormModel -> GhcField -> Maybe Text
ghcFieldError model field = either Just (const Nothing) $ case field of
  GhcBootstrapField -> void (parsedBootstrap model.bootstrapGhc)
  GhcHadrianField -> void (parsedHadrian model.hadrianGhc)
  GhcJobsField -> void (parsedJobs model.jobs)
  GhcBuildConfigField -> void (parsedBuildConfig model)
  GhcPatchesField -> void (parsedPatches model.patches)
  GhcOverwriteField -> void (parsedOverwrite model.overwriteVer)

canCompileGhc :: GhcFormModel -> Bool
canCompileGhc = isRight . toGhcOptions

toGhcOptions :: GhcFormModel -> Either Text CompileGhcOptions
toGhcOptions model = do
  bootstrapGhc <- parsedBootstrap model.bootstrapGhc
  hadrianGhc <- parsedHadrian model.hadrianGhc
  jobs <- parsedJobs model.jobs
  buildConfig <- parsedBuildConfig model
  patches <- parsedPatches model.patches
  overwriteVer <- parsedOverwrite model.overwriteVer
  pure
    Types.CompileGhcOptions
      { bootstrapGhc
      , hadrianGhc
      , jobs
      , buildConfig
      , patches
      , crossTarget = nonEmpty model.crossTarget
      , addConfArgs = words (Text.unpack model.addConfArgs)
      , setCompile = model.setCompile
      , overwriteVer
      , buildFlavour = Text.unpack <$> nonEmpty model.buildFlavour
      , buildSystem = model.buildSystem
      , isolateDir = model.isolateDir
      , gitRef = Text.unpack <$> nonEmpty model.gitRef
      , installTargets = case words (Text.unpack model.installTargets) of
          [] -> Nothing
          targets -> Just targets
      , docs = Text.unpack <$> nonEmpty model.docs
      }

parsedBootstrap :: Text -> Either Text (Either Version FilePath)
parsedBootstrap text = case nonEmpty text of
  Nothing -> Left "A bootstrap GHC version or path is required"
  Just input -> versionOrPath input

parsedHadrian :: Text -> Either Text (Maybe (Either Version FilePath))
parsedHadrian = whenEmpty Nothing (fmap Just . versionOrPath)

parsedBuildConfig :: GhcFormModel -> Either Text (Maybe FilePath)
parsedBuildConfig model = do
  path <-
    model.buildConfig
      & whenEmpty Nothing (\input -> Text.unpack input & absolutePathParser & bimap Text.pack Just)
  case (path, model.buildSystem) of
    (Just _, Just Hadrian) -> Left "A build config applies only to the make build system"
    _ -> Right path
