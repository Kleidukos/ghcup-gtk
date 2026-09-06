module Presentation.Banner
  ( BannerSpec (..)
  , BannerAction (..)
  , InstructionsSpec (..)
  , OfferFixSpec (..)
  ) where

import Data.Text (Text)

import Presentation.Row (Confirmation (..))

data BannerSpec = BannerSpec
  { title :: Text
  , actions :: [BannerAction]
  }
  deriving stock (Eq, Show)

data BannerAction
  = ShowInfo InstructionsSpec
  | ConfirmFix OfferFixSpec
  | OpenLog FilePath
  | Dismiss
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
