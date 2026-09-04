module Config
  ( Config (..)
  , ConfigUpdate (..)
  , SortColumn (..)
  , SortDirection (..)
  , TableFilters (..)
  , TableSort (..)
  , ViewMode (..)
  , applyUpdate
  , defaultConfig
  , parseConfig
  , parseConfigEither
  , renderConfig
  , load
  , save
  , sortColumnFromName
  , sortColumnName
  , viewMode
  ) where

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Scientific qualified as Scientific
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import KDL qualified
import System.Directory (XdgDirectory (..))
import System.FilePath ((</>))

import Effects.FileSystem

data SortColumn = ByVersion | ByReleased | ByStatus
  deriving stock (Eq, Show)

data SortDirection = Ascending | Descending
  deriving stock (Eq, Show)

data TableSort = TableSort
  { column :: SortColumn
  , direction :: SortDirection
  }
  deriving stock (Eq, Show)

data TableFilters = TableFilters
  { hlsPoweredOnly :: Bool
  , latestPatchOnly :: Bool
  }
  deriving stock (Eq, Show)

data ViewMode = Simple | Advanced
  deriving stock (Eq, Ord, Show)

data Config = Config
  { showOldVersions :: Bool
  , advancedInterface :: Bool
  , tableSort :: TableSort
  , tableFilters :: TableFilters
  , windowWidth :: Int
  , windowHeight :: Int
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { showOldVersions = False
    , advancedInterface = False
    , tableSort = TableSort ByVersion Descending
    , tableFilters = TableFilters False False
    , windowWidth = 760
    , windowHeight = 560
    }

viewMode :: Config -> ViewMode
viewMode config
  | config.advancedInterface = Advanced
  | otherwise = Simple

-- | A preference change, or a table-state change worth remembering.
data ConfigUpdate
  = SetShowOldVersions Bool
  | SetAdvancedInterface Bool
  | SetTableSort TableSort
  | SetTableFilters TableFilters
  | SetWindowSize Int Int
  deriving stock (Eq, Show)

applyUpdate :: ConfigUpdate -> Config -> Config
applyUpdate update config = case update of
  SetShowOldVersions b -> config {showOldVersions = b}
  SetAdvancedInterface b -> config {advancedInterface = b}
  SetTableSort sort -> config {tableSort = sort}
  SetTableFilters filters -> config {tableFilters = filters}
  SetWindowSize width height -> config {windowWidth = width, windowHeight = height}

parseConfigEither :: Text -> Either Text Config
parseConfigEither input = configOf <$> KDL.parse input
  where
    configOf doc =
      Config
        { showOldVersions = bool "show-old-versions" defaultConfig.showOldVersions doc
        , advancedInterface = bool "advanced-interface" defaultConfig.advancedInterface doc
        , tableSort =
            TableSort
              { column = fromMaybe defaultConfig.tableSort.column (sortColumn doc)
              , direction =
                  if bool "table-sort-descending" (defaultConfig.tableSort.direction == Descending) doc
                    then Descending
                    else Ascending
              }
        , tableFilters =
            TableFilters
              { hlsPoweredOnly = bool "filter-hls-powered" defaultConfig.tableFilters.hlsPoweredOnly doc
              , latestPatchOnly = bool "filter-latest-patch" defaultConfig.tableFilters.latestPatchOnly doc
              }
        , windowWidth = int "window-width" defaultConfig.windowWidth doc
        , windowHeight = int "window-height" defaultConfig.windowHeight doc
        }

    sortColumn doc = sortColumnFromName =<< stringArg "table-sort-column" doc

    bool name fallback doc = fromMaybe fallback (boolArg name doc)

    int name fallback doc = fromMaybe fallback (intArg name doc)

    boolArg name doc = case KDL.getArgAt name doc of
      Just KDL.Value {data_ = KDL.Bool b} -> Just b
      _ -> Nothing

    intArg name doc = case KDL.getArgAt name doc of
      Just KDL.Value {data_ = KDL.Number n} -> do
        value <- Scientific.toBoundedInteger n
        if value > 0 then Just value else Nothing
      _ -> Nothing

    stringArg name doc = case KDL.getArgAt name doc of
      Just KDL.Value {data_ = KDL.String s} -> Just s
      _ -> Nothing

parseConfig :: Text -> Config
parseConfig = fromRight defaultConfig . parseConfigEither

renderConfig :: Config -> Text
renderConfig config =
  KDL.render
    KDL.NodeList
      { nodes =
          [ boolNode "show-old-versions" config.showOldVersions
          , boolNode "advanced-interface" config.advancedInterface
          , stringNode "table-sort-column" (sortColumnName config.tableSort.column)
          , boolNode "table-sort-descending" (config.tableSort.direction == Descending)
          , boolNode "filter-hls-powered" config.tableFilters.hlsPoweredOnly
          , boolNode "filter-latest-patch" config.tableFilters.latestPatchOnly
          , intNode "window-width" config.windowWidth
          , intNode "window-height" config.windowHeight
          ]
      , ext = KDL.def
      }

sortColumnName :: SortColumn -> Text
sortColumnName = \case
  ByVersion -> "version"
  ByReleased -> "released"
  ByStatus -> "status"

sortColumnFromName :: Text -> Maybe SortColumn
sortColumnFromName = \case
  "version" -> Just ByVersion
  "released" -> Just ByReleased
  "status" -> Just ByStatus
  _ -> Nothing

boolNode :: Text -> Bool -> KDL.Node
boolNode name value = node name (KDL.Bool value)

stringNode :: Text -> Text -> KDL.Node
stringNode name value = node name (KDL.String value)

intNode :: Text -> Int -> KDL.Node
intNode name value = node name (KDL.Number (fromIntegral value))

node :: Text -> KDL.ValueData -> KDL.Node
node name value =
  KDL.Node
    { ann = Nothing
    , name = KDL.toIdentifier name
    , entries =
        [ KDL.Entry
            { name = Nothing
            , value = KDL.Value {ann = Nothing, data_ = value, ext = KDL.def}
            , ext = KDL.def
            }
        ]
    , children = Nothing
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
