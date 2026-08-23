module PresentationSpec (tests) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import GHCup.Command.List (ListResult)
import GHCup.Types (Tag (..))
import Test.Tasty
import Test.Tasty.HUnit

import Fixtures (dirs, listingsFor, lr914, mkLR, sampleChanges)
import Presentation
import Toolchain.Path (PathStatus (..), sourceLine)
import Toolchain.Types (Mutation (..), SupportedTool (..), keyOfMutation, reqOf, supportedTools, tvOf)

tests :: TestTree
tests =
  testGroup
    "Presentation"
    [ testGroup
        "tool confirmations"
        [ testCase "install: suggested, not destructive, names the version" $ do
            let spec = installConfirmation GHC lr914
            spec.heading @?= "Install GHC 9.14.1?"
            spec.body @?= "The download may take several minutes."
            spec.affirmLabel @?= "Install"
            spec.destructive @?= False
        , testCase "uninstall is destructive" $ do
            let spec = removeConfirmation GHC lr914
            spec.heading @?= "Uninstall GHC 9.14.1?"
            spec.body @?= "The files will be removed from your system."
            spec.affirmLabel @?= "Uninstall"
            spec.destructive @?= True
        ]
    , testGroup
        "path fix confirmation"
        [ testCase "not destructive, applies" $ do
            let spec = pathFixConfirmation sampleChanges
            spec.heading @?= "Set Up Your PATH?"
            spec.affirmLabel @?= "Apply"
            spec.destructive @?= False
        , testCase "body lists every touched file, tags the created env file" $ do
            let bodyLines = Text.lines (pathFixConfirmation sampleChanges).body
            ("/home/u/.ghcup/env (created, PATH setup)" `elem` bodyLines) @? "env file tagged"
            ("/home/u/.zshrc" `elem` bodyLines) @? "rc file listed untagged"
        , testCase "body shows appended lines but not the env file payload" $ do
            let bodyLines = Text.lines (pathFixConfirmation sampleChanges).body
            ("source env # ghcup-env" `elem` bodyLines) @? "appended payload shown"
            ("export PATH=..." `notElem` bodyLines) @? "created-file payload elided"
        ]
    , testGroup
        "pathBanner"
        [ testCase "PathOk shows nothing" $
            pathBanner dirs PathOk @?= Nothing
        , testCase "already fixed asks for a restart, no button" $
            pathBanner dirs FixedAwaitingRestart
              @?= Just
                (BannerSpec "Restart your terminal or session for the tools to be available" Nothing)
        , testCase "unknown shell gets copy-paste instructions with the exact source line" $
            case pathBanner dirs NeedsFixManual of
              Just (BannerSpec title (Just (ShowInfo spec))) -> do
                title @?= "Installed tools won't be found in your terminal"
                spec.buttonLabel @?= "How to fix…"
                spec.dialogHeading @?= "Add ghcup to your PATH"
                (sourceLine dirs `Text.isInfixOf` spec.dialogBody) @? "dialog body carries sourceLine"
              other -> assertFailure ("expected an instructions banner, got: " <> show other)
        , testCase "known shell gets the fix offer carrying the confirmation" $
            case pathBanner dirs (NeedsFixPlanned sampleChanges) of
              Just (BannerSpec title (Just (ConfirmFix spec))) -> do
                title @?= "Installed tools won't be found in your terminal"
                spec.buttonLabel @?= "Fix…"
                spec.confirmation @?= pathFixConfirmation sampleChanges
              other -> assertFailure ("expected a fix-offer banner, got: " <> show other)
        , testCase "applied banner asks for a restart" $
            appliedBanner @?= BannerSpec "Done — restart your terminal" Nothing
        ]
    , testGroup
        "planRows"
        [ testCase "row keys agree with the keys Session mints from mutations" $ do
            let specs = ghcRows True [lr914]
            ((.key) <$> Vector.toList specs)
              @?= [ keyOfMutation (Install GHC (reqOf lr914))
                  ]
            ((.key) <$> Vector.toList specs)
              @?= [ keyOfMutation (SetDefault GHC (tvOf lr914))
                  ]
        , testCase "curates: old untagged versions hidden unless asked for" $ do
            let old = mkLR "9.2.8" [] False False
            ((.title) <$> ghcRows False [lr914, old]) @?= Vector.fromList ["9.14.1"]
            ((.title) <$> ghcRows True [lr914, old]) @?= Vector.fromList ["9.14.1", "9.2.8"]
        , testCase "rows sorted descending by version" $ do
            let old = mkLR "9.2.8" [] False False
            ((.title) <$> ghcRows True [old, lr914]) @?= Vector.fromList ["9.14.1", "9.2.8"]
        , testCase "installed and default facts mirror the listing" $ do
            let inst = mkLR "9.10.3" [Recommended] True True
                specs = ghcRows False [lr914, inst]
            ((\s -> (s.installed, s.isDefault)) <$> specs)
              @?= Vector.fromList [(False, False), (True, True)]
        , testCase "pills from tags" $ do
            let inst = mkLR "9.10.3" [Recommended] True False
                specs = ghcRows False [lr914, inst]
            ((.pills) <$> specs) @?= Vector.fromList [["latest"], ["recommended"]]
        , testCase "row action is install or remove per installed state" $ do
            let inst = mkLR "9.10.3" [Recommended] True False
                specs = ghcRows False [lr914, inst]
            ((\s -> (s.action.label, s.action.job)) <$> specs)
              @?= Vector.fromList
                [ ("Install", Install GHC (reqOf lr914))
                , ("Remove", Uninstall GHC (tvOf inst))
                ]
            ((.setDefault) <$> specs)
              @?= Vector.fromList
                [ SetDefault GHC (tvOf lr914)
                , SetDefault GHC (tvOf inst)
                ]
        , testCase "subtitle names the default version" $ do
            let inst = mkLR "9.10.3" [Recommended] True True
            (ghcPlan False [lr914, inst]).subtitle @?= "Default: 9.10.3"
        , testCase "no default, no subtitle" $
            (ghcPlan False [lr914]).subtitle @?= ""
        , testCase "total over supportedTools, even on empty listings" $ do
            let plan = planRows False Map.empty
            Map.keys plan @?= sort (Vector.toList supportedTools)
            ((.rows) <$> Map.elems plan) @?= replicate 4 Vector.empty
        ]
    ]
  where
    ghcPlan :: Bool -> [ListResult] -> ToolRows
    ghcPlan showOld lrs =
      case Map.lookup GHC (planRows showOld (listingsFor GHC lrs)) of
        Just toolRows -> toolRows
        Nothing -> error "planRows lost the ghc entry"

    ghcRows :: Bool -> [ListResult] -> Vector.Vector RowSpec
    ghcRows showOld lrs = (ghcPlan showOld lrs).rows
