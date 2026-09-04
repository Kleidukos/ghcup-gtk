module PresentationSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..), Tool (..), cabal, ghc)
import Test.Tasty
import Test.Tasty.HUnit

import Fixtures (dirs, listingsFor, lr914, mkLR, sampleChanges)
import Presentation.Filter
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
            let spec = installConfirmation (Vector.head (ghcRows [lr914]))
            spec.heading @?= "Install GHC 9.14.1?"
            spec.body @?= "The download may take several minutes."
            spec.affirmLabel @?= "Install"
            spec.destructive @?= False
        , testCase "uninstall is destructive" $ do
            let spec = removeConfirmation (Vector.head (ghcRows [lr914]))
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
        [ testCase "row keys agree with the keys Session mints from mutations" $
            ((.key) <$> Vector.toList (ghcRows [lr914]))
              @?= [ keyOfMutation (Install ghc (reqOf lr914) defaultInstallOptions)
                  ]
        , testCase "old untagged versions stay in the plan: filtering is the views' job" $ do
            let old = mkLR "9.2.8" [] False False
            ((.title) <$> ghcRows [lr914, old]) @?= Vector.fromList ["9.14.1", "9.2.8"]
        , testCase "installed state drives facts, pills, action, and status label" $ do
            let dflt = mkLR "9.10.3" [Recommended] True True
                inst = mkLR "9.8.4" [] True False
                specs = ghcRows [lr914, dflt, inst]
            ((\s -> (s.installed, s.isDefault, statusLabel s)) <$> specs)
              @?= Vector.fromList
                [ (False, False, "")
                , (True, True, "default")
                , (True, False, "installed")
                ]
            ((.pills) <$> specs)
              @?= Vector.fromList [[LatestVersion], [RecommendedVersion], []]
            ((\s -> ((defaultAction s).label, (defaultAction s).job)) <$> specs)
              @?= Vector.fromList
                [ ("Install", Install ghc (reqOf lr914) defaultInstallOptions)
                , ("Remove", Uninstall ghc (tvOf dflt))
                , ("Remove", Uninstall ghc (tvOf inst))
                ]
            (setDefaultMutation <$> specs)
              @?= Vector.fromList
                [ SetDefault ghc (tvOf lr914)
                , SetDefault ghc (tvOf dflt)
                , SetDefault ghc (tvOf inst)
                ]
        , testCase "subtitle names the default version, empty without one" $ do
            let inst = mkLR "9.10.3" [Recommended] True True
            (ghcPlan [lr914, inst]).subtitle @?= "Default: 9.10.3"
            (ghcPlan [lr914]).subtitle @?= ""
        , testCase "keyed by the listings' own tools" $ do
            let listings =
                  Map.union
                    (listingsFor ghc [lr914])
                    (listingsFor (Tool "hlint") [mkLR "3.10" [Latest] False False])
                plan = planRows Map.empty listings
            Map.keys plan @?= Map.keys listings
            Map.keys (planRows Map.empty Map.empty) @?= []
        , testCase "rank is the newest-first position, so it sorts like the version" $ do
            let specs = ghcRows [mkLR "9.2.8" [] False False, lr914]
            ((\s -> (s.title, s.rank)) <$> Vector.toList specs)
              @?= [("9.14.1", 0), ("9.2.8", 1)]
        , testCase "release day mirrors the listing" $ do
            let dated = (mkLR "9.12.2" [] False False) {lReleaseDay = Just (fromGregorian 2025 3 22)}
                specs = ghcRows [dated, lr914]
            ((.releaseDay) <$> Vector.toList specs)
              @?= [Nothing, Just (fromGregorian 2025 3 22)]
        , testCase "latestInFamily carries curation's verdict onto the rows" $ do
            let specs = ghcRows [mkLR "9.12.2" [] False False, mkLR "9.12.1" [] False False]
            ((\s -> (s.title, s.latestInFamily)) <$> Vector.toList specs)
              @?= [("9.12.2", True), ("9.12.1", False)]
        , testCase "a busy map stamps progress onto the matching row" $ do
            let key = keyOfMutation (Install ghc (reqOf lr914) defaultInstallOptions)
                busy = Map.singleton key (Progress "unpacking" Nothing)
                specs = case Map.lookup ghc (planRows busy (listingsFor ghc [lr914])) of
                  Just toolRows -> toolRows.rows
                  Nothing -> error "planRows lost the ghc entry"
            ((.progress) <$> Vector.toList specs) @?= [Just (Progress "unpacking" Nothing)]
        , testCase "a stamp for a key outside the listings is dropped" $ do
            let busy = Map.singleton (keyOfMutation (Install cabal (reqOf lr914) defaultInstallOptions)) (Progress "x" Nothing)
                specs = case Map.lookup ghc (planRows busy (listingsFor ghc [lr914])) of
                  Just toolRows -> toolRows.rows
                  Nothing -> error "planRows lost the ghc entry"
            ((.progress) <$> Vector.toList specs) @?= [Nothing]
        , testCase "prerelease, nightly, and cross facts mirror the listing" $ do
            let pre = mkLR "9.15.0.20260801" [Prerelease] False False
                nightly = mkLR "9.15.20260830" [LatestNightly] False False
                crossed = (mkLR "9.14.1" [] False False) {lCross = Just "aarch64-linux"}
                specs = ghcRows [pre, nightly, crossed]
            ((\s -> (s.isPrerelease, s.isNightly, s.crossTarget)) <$> Vector.toList specs)
              @?= [ (False, True, Nothing)
                  , (True, False, Nothing)
                  , (False, False, Just "aarch64-linux")
                  ]
        , testCase "prerelease and nightly tags become pills" $ do
            let pre = mkLR "9.15.0.20260801" [Prerelease] False False
                latestPre = mkLR "9.15.0.20260802" [LatestPrerelease] False False
                nightly = mkLR "9.15.20260830" [Nightly] False False
                specs = ghcRows [latestPre, pre, nightly]
            ((.pills) <$> Vector.toList specs)
              @?= [[NightlyVersion], [PrereleaseVersion], [PrereleaseVersion]]
        , testCase "a cross row's title carries the target" $ do
            let crossed = (mkLR "9.14.1" [] False False) {lCross = Just "aarch64-linux"}
            ((.title) <$> Vector.toList (ghcRows [crossed, lr914]))
              @?= ["aarch64-linux-9.14.1", "9.14.1"]
        ]
    , testGroup
        "matchesFilters"
        [ testCase "no active filter shows only curated rows" $ do
            matchesFilters mempty (sampleSpec True) @?= True
            matchesFilters mempty (sampleSpec False) @?= False
            matchesFilters mempty ((sampleSpec True) {isPrerelease = True}) @?= False
            matchesFilters mempty ((sampleSpec True) {isNightly = True}) @?= False
            matchesFilters mempty ((sampleSpec True) {crossTarget = Just "aarch64-linux"}) @?= False
        , testCase "ShowOldPatches reveals older patches" $
            matchesFilters (kindFilters [ShowOldPatches]) (sampleSpec False) @?= True
        , testCase "channel filters reveal their channel" $ do
            matchesFilters (channelFilters [Prereleases]) ((sampleSpec True) {isPrerelease = True}) @?= True
            matchesFilters (channelFilters [Nightlies]) ((sampleSpec True) {isNightly = True}) @?= True
            matchesFilters (channelFilters [Cross]) ((sampleSpec True) {crossTarget = Just "aarch64-linux"}) @?= True
        , testCase "filters compose" $ do
            matchesFilters (channelFilters [Prereleases]) ((sampleSpec False) {isPrerelease = True}) @?= False
            matchesFilters (kindFilters [ShowOldPatches] <> channelFilters [Prereleases]) ((sampleSpec False) {isPrerelease = True}) @?= True
        ]
    ]
  where
    kindFilters ks = ActiveFilters (Set.fromList ks) Set.empty
    channelFilters cs = ActiveFilters Set.empty (Set.fromList cs)
    sampleSpec :: Bool -> RowSpec
    sampleSpec latest =
      (Vector.head (ghcRows [lr914])) {latestInFamily = latest}
    ghcPlan :: [ListResult] -> ToolRows
    ghcPlan lrs =
      case Map.lookup ghc (planRows Map.empty (listingsFor ghc lrs)) of
        Just toolRows -> toolRows
        Nothing -> error "planRows lost the ghc entry"

    ghcRows :: [ListResult] -> Vector.Vector RowSpec
    ghcRows lrs = (ghcPlan lrs).rows
