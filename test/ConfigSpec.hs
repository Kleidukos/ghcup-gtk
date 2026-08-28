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
    [ testGroup
        "list filters"
        [ testCase "parses the filters independently" $ do
            let c = parseConfig "list-filter-hls-powered #true\nlist-filter-latest-patch #false"
            c.listFilters @?= Filters True False
        , testCase "malformed document → the whole default config" $
            parseConfig "{{{{" @?= defaultConfig
        , testCase "v1-style bare bool is malformed → default" $
            (parseConfig "list-filter-hls-powered true").listFilters.hlsPoweredOnly @?= False
        , testCase "fresh install: list view unfiltered, table view fully filtered" $ do
            defaultConfig.listFilters @?= Filters False False
            defaultConfig.tableFilters @?= Filters True True
        , testCase "applyUpdate sets only the list filters" $ do
            let c = applyUpdate (SetListFilters (Filters True False)) defaultConfig
            c.listFilters @?= Filters True False
            c.tableFilters @?= defaultConfig.tableFilters
        ]
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
        [ testCase "parses the sort column and direction" $ do
            let c = parseConfig "table-sort-column \"released\"\ntable-sort-descending #false"
            c.tableSort @?= TableSort ByReleased Ascending
        , testCase "parses status as a sort column" $
            (parseConfig "table-sort-column \"status\"").tableSort.column @?= ByStatus
        , testCase "an unknown sort column falls back to version" $
            (parseConfig "table-sort-column \"colour\"").tableSort.column @?= ByVersion
        , testCase "parses the filters independently" $ do
            let c = parseConfig "filter-hls-powered #true\nfilter-latest-patch #false"
            c.tableFilters @?= Filters True False
        , testCase "round-trips every setting" $ do
            let c =
                  Config
                    { advancedInterface = True
                    , tableSort = TableSort ByStatus Ascending
                    , tableFilters = Filters True True
                    , listFilters = Filters True False
                    , windowWidth = 1024
                    , windowHeight = 768
                    }
            parseConfig (renderConfig c) @?= c
        , testCase "round-trips the defaults" $
            parseConfig (renderConfig defaultConfig) @?= defaultConfig
        , testCase "an empty document is exactly the defaults" $
            -- the "old config.kdl without the new keys" case: every missing
            -- node falls back, so an upgrade changes nothing
            parseConfig "" @?= defaultConfig
        , testCase "a retired show-old-versions node is ignored" $
            parseConfig "show-old-versions #true" @?= defaultConfig
        , testCase "a missing direction node keeps the default direction" $
            (parseConfig "table-sort-column \"released\"").tableSort
              @?= TableSort ByReleased Descending
        , testCase "applyUpdate touches only its own setting" $ do
            let sorted = applyUpdate (SetTableSort (TableSort ByReleased Ascending)) defaultConfig
            sorted.tableSort @?= TableSort ByReleased Ascending
            sorted.tableFilters @?= defaultConfig.tableFilters
            sorted.advancedInterface @?= False
            (applyUpdate (SetAdvancedInterface True) defaultConfig).advancedInterface @?= True
            (applyUpdate (SetTableFilters (Filters True False)) defaultConfig).tableFilters
              @?= Filters True False
        ]
    , testGroup
        "window size"
        [ testCase "parses both dimensions" $ do
            let c = parseConfig "window-width 1024\nwindow-height 768"
            (c.windowWidth, c.windowHeight) @?= (1024, 768)
        , testCase "missing nodes → defaults" $ do
            (parseConfig "").windowWidth @?= 960
            (parseConfig "").windowHeight @?= 560
        , testCase "a non-positive or non-integral dimension falls back" $ do
            (parseConfig "window-width -3").windowWidth @?= 960
            (parseConfig "window-width 12.5").windowWidth @?= 960
        , testCase "applyUpdate sets both dimensions" $ do
            let c = applyUpdate (SetWindowSize 800 600) defaultConfig
            (c.windowWidth, c.windowHeight) @?= (800, 600)
        ]
    , testGroup
        "load/save (pure filesystem)"
        [ testCase "missing file → defaults, no warning" $ do
            let ((config, warning), _) = runFs Map.empty load
            config @?= defaultConfig
            warning @?= Nothing
        , testCase "good file is read" $ do
            let files = Map.singleton configPath "list-filter-hls-powered #true"
                ((config, warning), _) = runFs files load
            config.listFilters.hlsPoweredOnly @?= True
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
            let c = defaultConfig {listFilters = Filters True False}
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
