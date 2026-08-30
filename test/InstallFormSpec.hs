module InstallFormSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Vector qualified as Vector
import GHCup.Command.List (ListResult)
import GHCup.Types (InstallDir (..), ghc)
import Test.Tasty
import Test.Tasty.HUnit

import Fixtures (lr914, mkLR)
import Presentation.InstallForm
import Presentation.Row (RowSpec, ToolRows (..), planRows)
import Toolchain.Types (InstallOptions (..), Listings, defaultInstallOptions)

specFor :: ListResult -> RowSpec
specFor lr =
  let listings = Map.singleton ghc (Vector.singleton lr) :: Listings
  in (planRows Map.empty listings Map.! ghc).rows Vector.! 0

freshModel :: FormModel
freshModel = initFormModel (specFor lr914)

installedModel :: FormModel
installedModel = initFormModel (specFor (mkLR "9.14.1" [] True True))

steps :: [FormEvent] -> FormModel -> FormModel
steps events model = foldl (flip stepForm) model events

tests :: TestTree
tests =
  testGroup
    "InstallForm"
    [ testCase "a fresh row preselects nothing" $ do
        freshModel.setDefault @?= False
        freshModel.force @?= False
        freshModel.isolate @?= Nothing
    , testCase "an installed default row seeds both switches" $ do
        installedModel.setDefault @?= True
        installedModel.force @?= True
    , testCase "picking an isolate dir locks set-default and force" $ do
        let model = stepForm (IsolatePicked "/opt/ghc") installedModel
        model.isolate @?= Just "/opt/ghc"
        effectiveSetDefault model @?= False
        effectiveForce model @?= False
        effectiveSetDefault (stepForm (SetDefaultToggled True) model) @?= False
        fmap (.installDir) (toOptions model) @?= Just (IsolateDir "/opt/ghc")
        fmap (.setAsDefault) (toOptions model) @?= Just False
        fmap (.forceInstall) (toOptions model) @?= Just False
    , testCase "clearing the isolate dir restores the prior switches" $
        steps [IsolatePicked "/opt/ghc", IsolateCleared] installedModel
          @?= installedModel
    , testCase "a malformed URL blocks install until corrected" $ do
        let bad = stepForm (UrlChanged "not a uri") freshModel
        assertBool "expected a url error" (isJust (urlError bad))
        canInstall bad @?= False
        toOptions bad @?= Nothing
        let good = stepForm (UrlChanged "") bad
        urlError good @?= Nothing
        canInstall good @?= True
    , testCase "toOptions: an empty form yields the defaults, a full form carries every field" $ do
        toOptions freshModel @?= Just defaultInstallOptions
        let model =
              steps
                [ ForceToggled True
                , ArgsChanged "  --with-x  --with-y "
                , TargetsChanged "install"
                ]
                freshModel
        toOptions model
          @?= Just
            defaultInstallOptions
              { forceInstall = True
              , extraConfArgs = ["--with-x", "--with-y"]
              , installTargets = Just ["install"]
              }
    ]
