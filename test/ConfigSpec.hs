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
        let c = Config{showOldVersions = True}
        (parseConfig (renderConfig c)).showOldVersions @?= True
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
            let c = Config{showOldVersions = True}
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
