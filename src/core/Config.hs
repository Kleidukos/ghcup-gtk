module Config
  ( Config (..)
  , ConfigUpdate (..)
  , applyUpdate
  , defaultConfig
  , parseConfigEither
  , renderConfig
  , load
  , save
  ) where

import Data.Maybe (fromMaybe)
import Data.Scientific qualified as Scientific
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import KDL qualified
import System.Directory (XdgDirectory (..))
import System.FilePath ((</>))

import Effects.FileSystem

data Config = Config
  { windowWidth :: Int
  , windowHeight :: Int
  , nightliesUrl :: Maybe Text
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { windowWidth = 960
    , windowHeight = 560
    , nightliesUrl = Nothing
    }

-- | A window-state change worth remembering.
data ConfigUpdate
  = SetWindowSize Int Int
  deriving stock (Eq, Show)

applyUpdate :: ConfigUpdate -> Config -> Config
applyUpdate update config = case update of
  SetWindowSize width height -> config {windowWidth = width, windowHeight = height}

parseConfigEither :: Text -> Either Text Config
parseConfigEither input = configOf <$> KDL.parse input
  where
    configOf doc =
      Config
        { windowWidth = int "window-width" defaultConfig.windowWidth doc
        , windowHeight = int "window-height" defaultConfig.windowHeight doc
        , nightliesUrl = textArg "nightlies-url" doc
        }

    int name fallback doc = fromMaybe fallback (intArg name doc)

    intArg name doc = case KDL.getArgAt name doc of
      Just KDL.Value {data_ = KDL.Number n} -> do
        value <- Scientific.toBoundedInteger n
        if value > 0 then Just value else Nothing
      _ -> Nothing

    textArg name doc = case KDL.getArgAt name doc of
      Just KDL.Value {data_ = KDL.String value}
        | not (Text.null (Text.strip value)) -> Just (Text.strip value)
      _ -> Nothing

renderConfig :: Config -> Text
renderConfig config =
  KDL.render
    KDL.NodeList
      { nodes =
          [ intNode "window-width" config.windowWidth
          , intNode "window-height" config.windowHeight
          ]
            <> maybe [] (\url -> [textNode "nightlies-url" url]) config.nightliesUrl
      , ext = KDL.def
      }

intNode :: Text -> Int -> KDL.Node
intNode name value = node name [KDL.Number (fromIntegral value)]

textNode :: Text -> Text -> KDL.Node
textNode name value = node name [KDL.String value]

node :: Text -> [KDL.ValueData] -> KDL.Node
node name values =
  KDL.Node
    { ann = Nothing
    , name = KDL.toIdentifier name
    , entries = entryOf <$> values
    , children = Nothing
    , ext = KDL.def
    }
  where
    entryOf value =
      KDL.Entry
        { name = Nothing
        , value = KDL.Value {ann = Nothing, data_ = value, ext = KDL.def}
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
