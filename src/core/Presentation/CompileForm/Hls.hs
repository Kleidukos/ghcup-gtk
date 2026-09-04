module Presentation.CompileForm.Hls
  ( HlsFormModel (..)
  , HlsFormEvent (..)
  , HlsField (..)
  , initHlsFormModel
  , stepHlsForm
  , hlsFieldError
  , canCompileHls
  , toHlsOptions
  ) where

import Control.Monad (void)
import Data.Bifunctor (bimap, first)
import Data.Either (isRight)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Versions (Version, prettyVer)
import GHCup.Input.Parsers (toolVersionEither, uriParser)
import URI.ByteString (URI)

import Presentation.CompileForm
import Toolchain.Types (CompileHlsOptions)
import Toolchain.Types qualified as Types

data HlsFormModel = HlsFormModel
  { targetGhcs :: Text
  , installedGhcs :: [Version]
  , jobs :: Text
  , setCompile :: Bool
  , priorSetCompile :: Bool
  , updateCabal :: Bool
  , overwriteVer :: Text
  , isolateDir :: Maybe FilePath
  , cabalProject :: Text
  , cabalProjectLocal :: Text
  , patches :: Text
  , cabalArgs :: Text
  , gitRef :: Text
  }
  deriving stock (Eq, Show)

data HlsFormEvent
  = HlsTargetGhcsChanged Text
  | HlsJobsChanged Text
  | HlsSetToggled Bool
  | HlsUpdateCabalToggled Bool
  | HlsOverwriteChanged Text
  | HlsIsolatePicked FilePath
  | HlsIsolateCleared
  | HlsCabalProjectChanged Text
  | HlsCabalProjectLocalChanged Text
  | HlsPatchesChanged Text
  | HlsCabalArgsChanged Text
  | HlsGitRefChanged Text
  deriving stock (Eq, Show)

initHlsFormModel :: [Version] -> HlsFormModel
initHlsFormModel installedGhcs =
  HlsFormModel
    { targetGhcs = Text.unwords (map prettyVer installedGhcs)
    , installedGhcs
    , jobs = ""
    , setCompile = False
    , priorSetCompile = False
    , updateCabal = False
    , overwriteVer = ""
    , isolateDir = Nothing
    , cabalProject = ""
    , cabalProjectLocal = ""
    , patches = ""
    , cabalArgs = ""
    , gitRef = ""
    }

stepHlsForm :: HlsFormEvent -> HlsFormModel -> HlsFormModel
stepHlsForm event model = case event of
  HlsTargetGhcsChanged text -> model {targetGhcs = text}
  HlsJobsChanged text -> model {jobs = text}
  HlsSetToggled b
    | isJust model.isolateDir -> model
    | otherwise -> model {setCompile = b, priorSetCompile = b}
  HlsUpdateCabalToggled b -> model {updateCabal = b}
  HlsOverwriteChanged text -> model {overwriteVer = text}
  HlsIsolatePicked path
    | isJust model.isolateDir -> model {isolateDir = Just path}
    | otherwise -> model {isolateDir = Just path, setCompile = False}
  HlsIsolateCleared -> model {isolateDir = Nothing, setCompile = model.priorSetCompile}
  HlsCabalProjectChanged text -> model {cabalProject = text}
  HlsCabalProjectLocalChanged text -> model {cabalProjectLocal = text}
  HlsPatchesChanged text -> model {patches = text}
  HlsCabalArgsChanged text -> model {cabalArgs = text}
  HlsGitRefChanged text -> model {gitRef = text}

data HlsField
  = HlsTargetGhcsField
  | HlsJobsField
  | HlsOverwriteField
  | HlsCabalProjectField
  | HlsCabalProjectLocalField
  | HlsPatchesField
  deriving stock (Eq, Show)

hlsFieldError :: HlsFormModel -> HlsField -> Maybe Text
hlsFieldError model field = either Just (const Nothing) $ case field of
  HlsTargetGhcsField -> void (parsedTargetGhcs model.installedGhcs model.targetGhcs)
  HlsJobsField -> void (parsedJobs model.jobs)
  HlsOverwriteField -> void (parsedOverwrite model.overwriteVer)
  HlsCabalProjectField -> void (parsedCabalProject model.cabalProject)
  HlsCabalProjectLocalField -> void (parsedCabalProjectLocal model.cabalProjectLocal)
  HlsPatchesField -> void (parsedPatches model.patches)

canCompileHls :: HlsFormModel -> Bool
canCompileHls = isRight . toHlsOptions

toHlsOptions :: HlsFormModel -> Either Text CompileHlsOptions
toHlsOptions model = do
  targetGhcs <- parsedTargetGhcs model.installedGhcs model.targetGhcs
  jobs <- parsedJobs model.jobs
  overwriteVer <- parsedOverwrite model.overwriteVer
  cabalProject <- parsedCabalProject model.cabalProject
  cabalProjectLocal <- parsedCabalProjectLocal model.cabalProjectLocal
  patches <- parsedPatches model.patches
  pure
    Types.CompileHlsOptions
      { targetGhcs
      , jobs
      , setCompile = model.setCompile
      , updateCabal = model.updateCabal
      , overwriteVer
      , isolateDir = model.isolateDir
      , cabalProject
      , cabalProjectLocal
      , patches
      , cabalArgs = Text.words model.cabalArgs
      , gitRef = Text.unpack <$> nonEmpty model.gitRef
      }

parsedTargetGhcs :: [Version] -> Text -> Either Text [Version]
parsedTargetGhcs installed text = case nonEmpty text of
  Nothing -> Left "At least one target GHC version is required"
  Just input -> do
    versions <-
      first Text.pack $
        traverse (toolVersionEither . Text.unpack) (Text.words input)
    case filter (`notElem` installed) versions of
      [] -> Right versions
      missing -> Left ("Not installed: GHC " <> Text.intercalate ", " (map prettyVer missing))

parsedCabalProject :: Text -> Either Text (Maybe (Either FilePath URI))
parsedCabalProject = whenEmpty Nothing $ \input ->
  case uriParser (Text.unpack input) of
    Right uri -> Right (Just (Right uri))
    Left _ -> Right (Just (Left (Text.unpack input)))

parsedCabalProjectLocal :: Text -> Either Text (Maybe URI)
parsedCabalProjectLocal = whenEmpty Nothing $ \input ->
  Text.unpack input & uriParser & bimap Text.pack Just
