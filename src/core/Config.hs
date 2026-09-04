module Config
  ( Config (..)
  , ConfigUpdate (..)
  , applyUpdate
  , defaultConfig
  , parseConfig
  , parseConfigEither
  , renderConfig
  , load
  , save
  ) where

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import KDL qualified
import System.Directory (XdgDirectory (..))
import System.FilePath ((</>))

import Effects.FileSystem

newtype Config = Config
  { showOldVersions :: Bool
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig = Config{showOldVersions = False}

-- | A preference change
data ConfigUpdate = SetShowOldVersions Bool
  deriving stock (Eq, Show)

applyUpdate :: ConfigUpdate -> Config -> Config
applyUpdate (SetShowOldVersions b) config = config{showOldVersions = b}

parseConfigEither :: Text -> Either Text Config
parseConfigEither input = configOf <$> KDL.parse input
  where
    configOf doc =
      Config
        { showOldVersions =
            fromMaybe defaultConfig.showOldVersions (boolArg "show-old-versions" doc)
        }
    boolArg name doc = case KDL.getArgAt name doc of
      Just KDL.Value{data_ = KDL.Bool b} -> Just b
      _ -> Nothing

parseConfig :: Text -> Config
parseConfig = fromRight defaultConfig . parseConfigEither

renderConfig :: Config -> Text
renderConfig config =
  KDL.render
    KDL.NodeList
      { nodes =
          [ KDL.Node
              { ann = Nothing
              , name = KDL.toIdentifier "show-old-versions"
              , entries =
                  [ KDL.Entry
                      { name = Nothing
                      , value =
                          KDL.Value
                            { ann = Nothing
                            , data_ = KDL.Bool config.showOldVersions
                            , ext = KDL.def
                            }
                      , ext = KDL.def
                      }
                  ]
              , children = Nothing
              , ext = KDL.def
              }
          ]
      , ext = KDL.def
      }

configFile :: (FileSystem :> es) => Eff es FilePath
configFile = do
  dir <- getXdgDirectory XdgConfig "ghcup-gtk"
  pure (dir </> "config.kdl")

load :: (FileSystem :> es) => Eff es (Config, Maybe Text)
load = do
  file <- configFile
  doesFileExist file >>= \case
    False -> pure (defaultConfig, Nothing)
    True ->
      readFileText file >>= \case
        Left e -> pure (defaultConfig, Just (warning file e))
        Right contents ->
          pure $ case parseConfigEither contents of
            Left err -> (defaultConfig, Just (warning file err))
            Right config -> (config, Nothing)
  where
    warning file err = "Ignoring malformed " <> Text.pack file <> ": " <> err

save :: (FileSystem :> es) => Config -> Eff es (Either Text ())
save config = do
  file <- configFile
  writeFileAtomic file (renderConfig config)
