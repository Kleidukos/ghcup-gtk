module PresentationSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..), Tool (..), cabal, ghc, ghcup)
import Test.Tasty
import Test.Tasty.HUnit

import Config (Filters (..))
import Fixtures (dirs, listingsFor, lr914, mkLR, sampleChanges)
import Presentation.Path
import Presentation.Row
import Toolchain.Path (PathStatus (..), sourceLine)
import Toolchain.Types (Mutation (..), Progress (..), defaultInstallOptions, keyOfMutation, reqOf, tvOf)

tests :: TestTree
tests =
  testGroup
    "Presentation"
    [ testGroup
        "tool confirmations"
        [ testCase "install: suggested, not destructive, names the version" $ do
            let spec = installConfirmation ghc lr914
            spec.heading @?= "Install GHC 9.14.1?"
            spec.body @?= "The download may take several minutes."
            spec.affirmLabel @?= "Install"
            spec.destructive @?= False
        , testCase "uninstall is destructive" $ do
            let spec = removeConfirmation ghc lr914
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
            appliedBanner @?= BannerSpec "Done – restart your terminal" Nothing
        ]
    , testGroup
        "planRows"
        [ testCase "row keys agree with the keys Session mints from mutations" $ do
            let specs = ghcRows [lr914]
            ((.key) <$> Vector.toList specs)
              @?= [ keyOfMutation (Install ghc (reqOf lr914) defaultInstallOptions)
                  ]
            ((.key) <$> Vector.toList specs)
              @?= [ keyOfMutation (SetDefault ghc (tvOf lr914))
                  ]
        , testCase "old untagged versions stay in the plan: filtering is the views' job" $ do
            let old = mkLR "9.2.8" [] False False
            ((.title) <$> ghcRows [lr914, old]) @?= Vector.fromList ["9.14.1", "9.2.8"]
        , testCase "installed and default facts mirror the listing" $ do
            let inst = mkLR "9.10.3" [Recommended] True True
                specs = ghcRows [lr914, inst]
            ((\s -> (s.installed, s.isDefault)) <$> specs)
              @?= Vector.fromList [(False, False), (True, True)]
        , testCase "pills from tags" $ do
            let inst = mkLR "9.10.3" [Recommended] True False
                specs = ghcRows [lr914, inst]
            ((.pills) <$> specs) @?= Vector.fromList [[LatestVersion], [RecommendedVersion]]
        , testCase "row action is install or remove per installed state" $ do
            let inst = mkLR "9.10.3" [Recommended] True False
                specs = ghcRows [lr914, inst]
            ((\s -> (s.action.label, s.action.job)) <$> specs)
              @?= Vector.fromList
                [ ("Install", Install ghc (reqOf lr914) defaultInstallOptions)
                , ("Remove", Uninstall ghc (tvOf inst))
                ]
            ((.setDefault) <$> specs)
              @?= Vector.fromList
                [ SetDefault ghc (tvOf lr914)
                , SetDefault ghc (tvOf inst)
                ]
        , testCase "subtitle names the default version" $ do
            let inst = mkLR "9.10.3" [Recommended] True True
            (ghcPlan [lr914, inst]).subtitle @?= "Default: 9.10.3"
        , testCase "no default, no subtitle" $
            (ghcPlan [lr914]).subtitle @?= ""
        , testCase "keyed by the listings' own tools" $ do
            let listings =
                  Map.union
                    (listingsFor ghc [lr914])
                    (listingsFor (Tool "hlint") [mkLR "3.10" [Latest] False False])
                plan = planRows Map.empty listings
            Map.keys plan @?= Map.keys listings
        , testCase "empty listings produce an empty plan" $
            Map.keys (planRows Map.empty Map.empty) @?= []
        , testCase "rank is the newest-first position, so it sorts like the version" $ do
            let specs = ghcRows [mkLR "9.2.8" [] False False, lr914]
            ((\s -> (s.title, s.rank)) <$> Vector.toList specs)
              @?= [("9.14.1", 0), ("9.2.8", 1)]
        , testCase "release day and hls-powered mirror the listing" $ do
            let dated =
                  (mkLR "9.12.2" [] False False)
                    { lReleaseDay = Just (fromGregorian 2025 3 22)
                    , hlsPowered = True
                    }
                specs = ghcRows [dated, lr914]
            ((\s -> (s.releaseDay, s.passesHlsFilter)) <$> Vector.toList specs)
              @?= [(Nothing, False), (Just (fromGregorian 2025 3 22), True)]
        , testCase "latestInFamily marks one row per major.minor" $ do
            let rows =
                  [ mkLR "9.12.2" [] False False
                  , mkLR "9.12.1" [] False False
                  , mkLR "9.10.1" [] False False
                  ]
                specs = ghcRows rows
            ((\s -> (s.title, s.latestInFamily)) <$> Vector.toList specs)
              @?= [("9.12.2", True), ("9.12.1", False), ("9.10.1", True)]
        , testCase "statusLabel names the row's state" $ do
            let dflt = mkLR "9.10.3" [] True True
                inst = mkLR "9.8.4" [] True False
                specs = ghcRows [lr914, dflt, inst]
            -- newest-first: 9.14.1 (neither), 9.10.3 (default), 9.8.4 (installed)
            ((.statusLabel) <$> Vector.toList specs) @?= ["", "default", "installed"]
        , testCase "non-GHC tools always count as hls-powered" $ do
            let cabalRow = mkLR "3.14.1.0" [Latest] False False
                specs = case Map.lookup cabal (planRows Map.empty (listingsFor cabal [cabalRow])) of
                  Just toolRows -> toolRows.rows
                  Nothing -> error "planRows lost the cabal entry"
            ((.passesHlsFilter) <$> Vector.toList specs) @?= [True]
        , testCase "a busy map stamps progress onto the matching row" $ do
            let key = keyOfMutation (Install ghc (reqOf lr914) defaultInstallOptions)
                busy = Map.singleton key (Progress "unpacking" Nothing)
                specs = case Map.lookup ghc (planRows busy (listingsFor ghc [lr914])) of
                  Just toolRows -> toolRows.rows
                  Nothing -> error "planRows lost the ghc entry"
            ((.progress) <$> Vector.toList specs) @?= [Just (Progress "unpacking" Nothing)]
        , testCase "rows without a stamp carry Nothing" $
            ((.progress) <$> Vector.toList (ghcRows [lr914])) @?= [Nothing]
        , testCase "a stamp for a key outside the listings is dropped" $ do
            let busy = Map.singleton (keyOfMutation (Install cabal (reqOf lr914) defaultInstallOptions)) (Progress "x" Nothing)
                specs = case Map.lookup ghc (planRows busy (listingsFor ghc [lr914])) of
                  Just toolRows -> toolRows.rows
                  Nothing -> error "planRows lost the ghc entry"
            ((.progress) <$> Vector.toList specs) @?= [Nothing]
        ]
    , testGroup
        "matchesFilters"
        [ testCase "no active filter keeps every row" $
            matchesFilters (Filters False False) (sampleSpec False False) @? "kept"
        , testCase "hls filter drops rows that fail it" $ do
            matchesFilters (Filters True False) (sampleSpec False True) @?= False
            matchesFilters (Filters True False) (sampleSpec True True) @?= True
        , testCase "latest-patch filter drops older patches" $ do
            matchesFilters (Filters False True) (sampleSpec True False) @?= False
            matchesFilters (Filters False True) (sampleSpec True True) @?= True
        , testCase "both filters must pass together" $
            matchesFilters (Filters True True) (sampleSpec False True) @?= False
        ]
    ]
  where
    sampleSpec :: Bool -> Bool -> RowSpec
    sampleSpec hls latest =
      (Vector.head (ghcRows [lr914])) {passesHlsFilter = hls, latestInFamily = latest}
    ghcPlan :: [ListResult] -> ToolRows
    ghcPlan lrs =
      case Map.lookup ghc (planRows Map.empty (listingsFor ghc lrs)) of
        Just toolRows -> toolRows
        Nothing -> error "planRows lost the ghc entry"

    ghcRows :: [ListResult] -> Vector.Vector RowSpec
    ghcRows lrs = (ghcPlan lrs).rows
