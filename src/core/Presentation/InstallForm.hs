module Presentation.InstallForm
  ( FormModel (..)
  , FormEvent (..)
  , initFormModel
  , stepForm
  , urlError
  , canInstall
  , effectiveSetDefault
  , effectiveForce
  , toOptions
  ) where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Maybe (isNothing)
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
    , url = ""
    , extraArgs = ""
    , targets = ""
    }

stepForm :: FormEvent -> FormModel -> FormModel
stepForm event model = case event of
  UrlChanged text -> model {url = text}
  IsolatePicked path -> model {isolate = Just path}
  IsolateCleared -> model {isolate = Nothing}
  SetDefaultToggled b -> model {setDefault = b}
  ForceToggled b -> model {force = b}
  ArgsChanged text -> model {extraArgs = text}
  TargetsChanged text -> model {targets = text}

effectiveSetDefault :: FormModel -> Bool
effectiveSetDefault model = isNothing model.isolate && model.setDefault

effectiveForce :: FormModel -> Bool
effectiveForce model = isNothing model.isolate && model.force

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
        { setAsDefault = effectiveSetDefault model
        , forceInstall = effectiveForce model
        , installDir = maybe GHCupInternal IsolateDir model.isolate
        , bindistUrl
        , extraConfArgs = words (Text.unpack model.extraArgs)
        , installTargets = case words (Text.unpack model.targets) of
            [] -> Nothing
            ws -> Just ws
        }
