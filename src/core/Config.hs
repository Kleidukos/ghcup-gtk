module Config
  ( Config (..)
  , ConfigUpdate (..)
  , SortColumn (..)
  , SortDirection (..)
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
  ) where

import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific qualified as Scientific
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import GHCup.Types (Tool (..))
import KDL qualified
import System.Directory (XdgDirectory (..))
import System.FilePath ((</>))

import Effects.FileSystem
import Presentation.Filter (FilterKind, ToolFilters, filterFromName, filterName)
import Toolchain.Types (toolText)

data SortColumn = ByVersion | ByReleased | ByStatus
  deriving stock (Eq, Show)

data SortDirection = Ascending | Descending
  deriving stock (Eq, Show)

data TableSort = TableSort
  { column :: SortColumn
  , direction :: SortDirection
  }
  deriving stock (Eq, Show)

data ViewMode = Simple | Advanced
  deriving stock (Eq, Ord, Show)

data Config = Config
  { viewMode :: ViewMode
  , tableSort :: TableSort
  , toolFilters :: ToolFilters
  , windowWidth :: Int
  , windowHeight :: Int
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { viewMode = Simple
    , tableSort = TableSort ByVersion Descending
    , toolFilters = Map.empty
    , windowWidth = 960
    , windowHeight = 560
    }

-- | A preference change, or a table-state change worth remembering.
data ConfigUpdate
  = SetViewMode ViewMode
  | SetTableSort TableSort
  | SetToolFilters Tool (Set FilterKind)
  | SetWindowSize Int Int
  deriving stock (Eq, Show)

applyUpdate :: ConfigUpdate -> Config -> Config
applyUpdate update config = case update of
  SetViewMode mode -> config {viewMode = mode}
  SetTableSort sort -> config {tableSort = sort}
  SetToolFilters tool filters -> config {toolFilters = Map.insert tool filters config.toolFilters}
  SetWindowSize width height -> config {windowWidth = width, windowHeight = height}

parseConfigEither :: Text -> Either Text Config
parseConfigEither input = configOf <$> KDL.parse input
  where
    configOf doc =
      Config
        { viewMode = fromMaybe defaultConfig.viewMode (viewModeFromName =<< stringArg "view-mode" doc)
        , tableSort =
            TableSort
              { column = fromMaybe defaultConfig.tableSort.column (sortColumn doc)
              , direction =
                  if bool "table-sort-descending" (defaultConfig.tableSort.direction == Descending) doc
                    then Descending
                    else Ascending
              }
        , toolFilters = toolFiltersOf doc
        , windowWidth = int "window-width" defaultConfig.windowWidth doc
        , windowHeight = int "window-height" defaultConfig.windowHeight doc
        }

    sortColumn doc = sortColumnFromName =<< stringArg "table-sort-column" doc

    toolFiltersOf doc =
      Map.fromList (mapMaybe toolFiltersNode (KDL.filterNodes "tool-filters" doc))

    toolFiltersNode n = case n.entries of
      KDL.Entry {name = Nothing, value = KDL.Value {data_ = KDL.String toolName}} : rest ->
        Just (Tool (Text.unpack toolName), Set.fromList (mapMaybe filterFromName (argStrings rest)))
      _ -> Nothing

    argStrings entries =
      [s | KDL.Entry {name = Nothing, value = KDL.Value {data_ = KDL.String s}} <- entries]

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
          [ stringNode "view-mode" (viewModeName config.viewMode)
          , stringNode "table-sort-column" (sortColumnName config.tableSort.column)
          , boolNode "table-sort-descending" (config.tableSort.direction == Descending)
          ]
            <> toolFilterNodes config.toolFilters
            <> [ intNode "window-width" config.windowWidth
               , intNode "window-height" config.windowHeight
               ]
      , ext = KDL.def
      }

viewModeName :: ViewMode -> Text
viewModeName = \case
  Simple -> "simple"
  Advanced -> "advanced"

viewModeFromName :: Text -> Maybe ViewMode
viewModeFromName = \case
  "simple" -> Just Simple
  "advanced" -> Just Advanced
  _ -> Nothing

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

toolFilterNodes :: ToolFilters -> [KDL.Node]
toolFilterNodes toolFilters =
  [ node "tool-filters" (KDL.String <$> toolText tool : map filterName (Set.toAscList filters))
  | (tool, filters) <- Map.toAscList toolFilters
  ]

boolNode :: Text -> Bool -> KDL.Node
boolNode name value = node name [KDL.Bool value]

stringNode :: Text -> Text -> KDL.Node
stringNode name value = node name [KDL.String value]

intNode :: Text -> Int -> KDL.Node
intNode name value = node name [KDL.Number (fromIntegral value)]

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
