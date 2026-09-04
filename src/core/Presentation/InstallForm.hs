module Presentation.InstallForm
  ( FormModel (..)
  , FormEvent (..)
  , initFormModel
  , stepForm
  , urlError
  , canInstall
  , setDefaultLocked
  , toOptions
  ) where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import GHCup.Input.Parsers (uriParser)
import GHCup.Types (InstallDir (..))
import URI.ByteString (URI)

import Presentation.Row (RowSpec (..))
import Toolchain.Types (InstallOptions (..), defaultInstallOptions)

data FormModel = FormModel
  { setDefault :: Bool
  , force :: Bool
  , isolate :: Maybe FilePath
  , priorSetDefault :: Bool
  , priorForce :: Bool
  , url :: Text
  , extraArgs :: Text
  , targets :: Text
  }
  deriving stock (Eq, Show)

data FormEvent
  = UrlChanged Text
  | IsolatePicked FilePath
  | IsolateCleared
  | SetDefaultToggled Bool
  | ForceToggled Bool
  | ArgsChanged Text
  | TargetsChanged Text
  deriving stock (Eq, Show)

initFormModel :: RowSpec -> FormModel
initFormModel spec =
  FormModel
    { setDefault = spec.isDefault
    , force = spec.installed
    , isolate = Nothing
    , priorSetDefault = spec.isDefault
    , priorForce = spec.installed
    , url = ""
    , extraArgs = ""
    , targets = ""
    }

stepForm :: FormEvent -> FormModel -> FormModel
stepForm event model = case event of
  UrlChanged text -> model {url = text}
  IsolatePicked path
    | isJust model.isolate -> model {isolate = Just path}
    | otherwise ->
        model
          { isolate = Just path
          , setDefault = False
          , force = False
          }
  IsolateCleared ->
    model
      { isolate = Nothing
      , setDefault = model.priorSetDefault
      , force = model.priorForce
      }
  SetDefaultToggled b
    | setDefaultLocked model -> model
    | otherwise -> model {setDefault = b, priorSetDefault = b}
  ForceToggled b
    | isJust model.isolate -> model {force = b}
    | otherwise -> model {force = b, priorForce = b}
  ArgsChanged text -> model {extraArgs = text}
  TargetsChanged text -> model {targets = text}

setDefaultLocked :: FormModel -> Bool
setDefaultLocked model = isJust model.isolate

parsedUrl :: FormModel -> Either Text (Maybe URI)
parsedUrl model
  | Text.null stripped = Right Nothing
  | otherwise = Text.unpack stripped & uriParser & bimap Text.pack Just
  where
    stripped = Text.strip model.url

urlError :: FormModel -> Maybe Text
urlError = either Just (const Nothing) . parsedUrl

canInstall :: FormModel -> Bool
canInstall = either (const False) (const True) . parsedUrl

toOptions :: FormModel -> Maybe InstallOptions
toOptions model = case parsedUrl model of
  Left _ -> Nothing
  Right bindistUrl ->
    Just
      defaultInstallOptions
        { setAsDefault = model.setDefault
        , forceInstall = model.force
        , installDir = maybe GHCupInternal IsolateDir model.isolate
        , bindistUrl
        , extraConfArgs = words (Text.unpack model.extraArgs)
        , installTargets = case words (Text.unpack model.targets) of
            [] -> Nothing
            ws -> Just ws
        }
