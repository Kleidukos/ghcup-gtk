module CompileFormSpec (tests) where

import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Versions (Version, version)
import GHCup.Types (BuildSystem (..))
import Test.Tasty
import Test.Tasty.HUnit

import Fixtures (defaultCompileGhcOptions, defaultCompileHlsOptions)
import Presentation.CompileForm.Ghc
import Presentation.CompileForm.Hls
import Toolchain.Types (CompileGhcOptions (..), CompileHlsOptions (..))

mkV :: Text -> Version
mkV text = version text & either (error . show) id

ghcSteps :: [GhcFormEvent] -> GhcFormModel -> GhcFormModel
ghcSteps events model = foldl (flip stepGhcForm) model events

hlsSteps :: [HlsFormEvent] -> HlsFormModel -> HlsFormModel
hlsSteps events model = foldl (flip stepHlsForm) model events

freshGhc :: GhcFormModel
freshGhc = initGhcFormModel []

seededGhc :: GhcFormModel
seededGhc = initGhcFormModel [mkV "9.6.5", mkV "9.14.1", mkV "9.10.1"]

tests :: TestTree
tests =
  testGroup
    "CompileForm"
    [ testGroup
        "GHC"
        [ testCase "the newest installed GHC seeds the bootstrap field" $ do
            seededGhc.bootstrapGhc @?= "9.14.1"
            toGhcOptions seededGhc @?= Right (defaultCompileGhcOptions (Left (mkV "9.14.1")))
        , testCase "an empty bootstrap blocks compilation" $ do
            assertBool "expected an error" (isJust (ghcFieldError freshGhc GhcBootstrapField))
            canCompileGhc freshGhc @?= False
        , testCase "the bootstrap accepts a version or an absolute path" $ do
            let byVersion = stepGhcForm (GhcBootstrapChanged "9.6.5") freshGhc
            fmap (.bootstrapGhc) (toGhcOptions byVersion) @?= Right (Left (mkV "9.6.5"))
            let byPath = stepGhcForm (GhcBootstrapChanged "/usr/bin/ghc") freshGhc
            fmap (.bootstrapGhc) (toGhcOptions byPath) @?= Right (Right "/usr/bin/ghc")
        , testCase "jobs must be a positive integer" $ do
            let bad = stepGhcForm (GhcJobsChanged "many") seededGhc
            assertBool "expected an error" (isJust (ghcFieldError bad GhcJobsField))
            canCompileGhc bad @?= False
            let good = stepGhcForm (GhcJobsChanged "8") seededGhc
            fmap (.jobs) (toGhcOptions good) @?= Right (Just 8)
        , testCase "a build config is rejected under the hadrian build system" $ do
            let model =
                  ghcSteps
                    [ GhcBuildConfigChanged "/tmp/build.mk"
                    , GhcBuildSystemChanged (Just Hadrian)
                    ]
                    seededGhc
            assertBool "expected an error" (isJust (ghcFieldError model GhcBuildConfigField))
            let make = stepGhcForm (GhcBuildSystemChanged (Just Make)) model
            fmap (.buildConfig) (toGhcOptions make) @?= Right (Just "/tmp/build.mk")
        , testCase "picking an isolate dir clears and locks set" $ do
            let model = ghcSteps [GhcSetToggled True, GhcIsolatePicked "/opt/ghc"] seededGhc
            model.setCompile @?= False
            model.isolateDir @?= Just "/opt/ghc"
            (stepGhcForm (GhcSetToggled True) model).setCompile @?= False
            let cleared = stepGhcForm GhcIsolateCleared model
            cleared.setCompile @?= True
            cleared.isolateDir @?= Nothing
        , testCase "patches accept a directory or URLs" $ do
            let byDir = stepGhcForm (GhcPatchesChanged "/opt/patches") seededGhc
            fmap (fmap (either Just (const Nothing)) . (.patches)) (toGhcOptions byDir)
              @?= Right (Just (Just "/opt/patches"))
            let bad = stepGhcForm (GhcPatchesChanged "relative/dir") seededGhc
            assertBool "expected an error" (isJust (ghcFieldError bad GhcPatchesField))
        , testCase "toGhcOptions carries every field" $ do
            let model =
                  ghcSteps
                    [ GhcHadrianChanged "9.10.1"
                    , GhcJobsChanged "4"
                    , GhcConfArgsChanged " --enable-x  --enable-y "
                    , GhcCrossTargetChanged "aarch64-linux"
                    , GhcFlavourChanged "quickest"
                    , GhcDocsChanged "none"
                    , GhcGitRefChanged "master"
                    , GhcInstallTargetsChanged "install"
                    , GhcSetToggled True
                    ]
                    seededGhc
                opts = toGhcOptions model
            fmap (.hadrianGhc) opts @?= Right (Just (Left (mkV "9.10.1")))
            fmap (.addConfArgs) opts @?= Right ["--enable-x", "--enable-y"]
            fmap (.crossTarget) opts @?= Right (Just "aarch64-linux")
            fmap (.buildFlavour) opts @?= Right (Just "quickest")
            fmap (.docs) opts @?= Right (Just "none")
            fmap (.gitRef) opts @?= Right (Just "master")
            fmap (.installTargets) opts @?= Right (Just ["install"])
            fmap (.setCompile) opts @?= Right True
        ]
    , testGroup
        "HLS"
        [ testCase "installed GHCs seed the target field" $ do
            let model = initHlsFormModel [mkV "9.6.5", mkV "9.10.1"]
            model.targetGhcs @?= "9.6.5 9.10.1"
            toHlsOptions model @?= Right (defaultCompileHlsOptions [mkV "9.6.5", mkV "9.10.1"])
        , testCase "no target GHCs blocks compilation" $ do
            let model = initHlsFormModel []
            assertBool "expected an error" (isJust (hlsFieldError model HlsTargetGhcsField))
            canCompileHls model @?= False
        , testCase "a malformed target version blocks compilation" $ do
            let model = stepHlsForm (HlsTargetGhcsChanged "9.6.5 !!!") (initHlsFormModel [])
            assertBool "expected an error" (isJust (hlsFieldError model HlsTargetGhcsField))
        , testCase "picking an isolate dir clears and locks set" $ do
            let model =
                  hlsSteps
                    [HlsSetToggled True, HlsIsolatePicked "/opt/hls"]
                    (initHlsFormModel [mkV "9.6.5"])
            model.setCompile @?= False
            (stepHlsForm HlsIsolateCleared model).setCompile @?= True
        , testCase "a malformed cabal.project.local URL blocks compilation" $ do
            let model =
                  stepHlsForm
                    (HlsCabalProjectLocalChanged "not a url")
                    (initHlsFormModel [mkV "9.6.5"])
            assertBool "expected an error" (isJust (hlsFieldError model HlsCabalProjectLocalField))
            canCompileHls model @?= False
        , testCase "toHlsOptions carries every field" $ do
            let model =
                  hlsSteps
                    [ HlsJobsChanged "2"
                    , HlsUpdateCabalToggled True
                    , HlsCabalArgsChanged " --allow-newer  -f foo "
                    , HlsCabalProjectChanged "cabal.project"
                    , HlsGitRefChanged "wip"
                    , HlsSetToggled True
                    ]
                    (initHlsFormModel [mkV "9.6.5"])
                opts = toHlsOptions model
            fmap (.jobs) opts @?= Right (Just 2)
            fmap (.updateCabal) opts @?= Right True
            fmap (.cabalArgs) opts @?= Right ["--allow-newer", "-f", "foo"]
            fmap (.cabalProject) opts @?= Right (Just (Left "cabal.project"))
            fmap (.gitRef) opts @?= Right (Just "wip")
            fmap (.setCompile) opts @?= Right True
        ]
    ]
