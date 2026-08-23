module ConfigSpec (tests) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Test.Tasty
import Test.Tasty.HUnit

import Config
import Effects.FileSystem (FileSystem)
import TestInterpreters (runFileSystemPure)

configPath :: FilePath
configPath = "/fake/xdg/ghcup-gtk/config.kdl"

runFs :: Map FilePath Text -> Eff '[FileSystem] a -> (a, Map FilePath Text)
runFs files = runPureEff . runFileSystemPure Map.empty files []

tests :: TestTree
tests =
  testGroup
    "Config"
    [ testCase "parses show-old-versions #true" $
        (parseConfig "show-old-versions #true").showOldVersions @?= True
    , testCase "parses show-old-versions #false" $
        (parseConfig "show-old-versions #false").showOldVersions @?= False
    , testCase "missing node → default" $
        (parseConfig "").showOldVersions @?= False
    , testCase "malformed document → default" $
        (parseConfig "{{{{").showOldVersions @?= False
    , testCase "v1-style bare bool is malformed → default" $
        (parseConfig "show-old-versions true").showOldVersions @?= False
    , testCase "round-trip" $ do
        let c = defaultConfig {showOldVersions = True}
        (parseConfig (renderConfig c)).showOldVersions @?= True
    , testGroup
        "advanced interface"
        [ testCase "parses advanced-interface #true" $
            (parseConfig "advanced-interface #true").advancedInterface @?= True
        , testCase "missing node → simple interface" $
            (parseConfig "").advancedInterface @?= False
        , testCase "viewMode follows the flag" $ do
            viewMode defaultConfig @?= Simple
            viewMode defaultConfig {advancedInterface = True} @?= Advanced
        ]
    , testGroup
        "table state"
        [ testCase "defaults are version-descending, no filters" $ do
            defaultConfig.tableSort @?= TableSort ByVersion Descending
            defaultConfig.tableFilters @?= TableFilters False False
        , testCase "parses the sort column and direction" $ do
            let c = parseConfig "table-sort-column \"released\"\ntable-sort-descending #false"
            c.tableSort @?= TableSort ByReleased Ascending
        , testCase "parses status as a sort column" $
            (parseConfig "table-sort-column \"status\"").tableSort.column @?= ByStatus
        , testCase "an unknown sort column falls back to version" $
            (parseConfig "table-sort-column \"colour\"").tableSort.column @?= ByVersion
        , testCase "parses the filters independently" $ do
            let c = parseConfig "filter-hls-powered #true\nfilter-latest-patch #false"
            c.tableFilters @?= TableFilters True False
        , testCase "round-trips every setting" $ do
            let c =
                  Config
                    { showOldVersions = True
                    , advancedInterface = True
                    , tableSort = TableSort ByStatus Ascending
                    , tableFilters = TableFilters True True
                    }
            parseConfig (renderConfig c) @?= c
        , testCase "round-trips the defaults" $
            parseConfig (renderConfig defaultConfig) @?= defaultConfig
        , testCase "an empty document is exactly the defaults" $
            -- the "old config.kdl without the new keys" case: every missing
            -- node falls back, so an upgrade changes nothing
            parseConfig "" @?= defaultConfig
        , testCase "a missing direction node keeps the default direction" $
            (parseConfig "table-sort-column \"released\"").tableSort
              @?= TableSort ByReleased Descending
        , testCase "applyUpdate touches only its own setting" $ do
            let sorted = applyUpdate (SetTableSort (TableSort ByReleased Ascending)) defaultConfig
            sorted.tableSort @?= TableSort ByReleased Ascending
            sorted.tableFilters @?= defaultConfig.tableFilters
            sorted.advancedInterface @?= False
            (applyUpdate (SetAdvancedInterface True) defaultConfig).advancedInterface @?= True
            (applyUpdate (SetTableFilters (TableFilters True False)) defaultConfig).tableFilters
              @?= TableFilters True False
        ]
    , testGroup
        "load/save (pure filesystem)"
        [ testCase "missing file → defaults, no warning" $ do
            let ((config, warning), _) = runFs Map.empty load
            config @?= defaultConfig
            warning @?= Nothing
        , testCase "good file is read" $ do
            let files = Map.singleton configPath "show-old-versions #true"
                ((config, warning), _) = runFs files load
            config.showOldVersions @?= True
            warning @?= Nothing
        , testCase "malformed file → defaults plus a warning naming the file" $ do
            let files = Map.singleton configPath "{{{{"
                ((config, warning), _) = runFs files load
            config @?= defaultConfig
            isJust warning @? "expected a warning"
            maybe False (Text.pack configPath `Text.isInfixOf`) warning
              @? "warning names the config file"
        , testCase "unreadable file → defaults plus a warning" $ do
            let ((config, warning), _) =
                  runPureEff (runFileSystemPure Map.empty Map.empty [configPath] load)
            config @?= defaultConfig
            isJust warning @? "expected a warning"
        , testCase "save-then-load round-trip" $ do
            let c = defaultConfig {showOldVersions = True}
                ((saved, (loaded, warning)), _) =
                  runFs Map.empty $ do
                    saveResult <- save c
                    loadedAndWarning <- load
                    pure (saveResult, loadedAndWarning)
            saved @?= Right ()
            loaded @?= c
            warning @?= Nothing
        ]
    ]
