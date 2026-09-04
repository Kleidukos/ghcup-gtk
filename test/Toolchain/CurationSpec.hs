module Toolchain.CurationSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Versions (Version)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..), Tool, cabal, ghc)
import Test.Tasty
import Test.Tasty.HUnit

import Fixtures (listingsFor, mkLR)
import Toolchain.Curation
import Toolchain.Types (Listings)

versionsOf :: Tool -> Listings -> [Version]
versionsOf tool listings =
  map lVer (Vector.toList (Map.findWithDefault Vector.empty tool listings))

tests :: TestTree
tests =
  testGroup
    "Curation"
    [ testCase "keeps every installable row" $ do
        let rows =
              [ mkLR "9.14.1" [Latest] False False
              , mkLR "9.12.1" [] False False
              , mkLR "8.10.7" [] False False
              ]
        length (versionsOf ghc (curate (listingsFor ghc rows))) @?= 3
    , testCase "sorts descending by version" $ do
        let rows =
              [ mkLR "3.10.3.0" [] True False
              , mkLR "3.14.1.0" [Latest] False False
              ]
        versionsOf cabal (curate (listingsFor cabal rows))
          @?= map lVer [rows !! 1, head rows]
    , testCase "tools stay separate" $ do
        let listings =
              Map.fromList
                [ (ghc, Vector.fromList [mkLR "9.14.1" [Latest] False False])
                , (cabal, Vector.fromList [mkLR "3.14.1.0" [Latest] False False])
                ]
        Map.keys (curate listings) @?= Map.keys listings
    , testCase "empty input, empty output" $
        curate Map.empty @?= Map.empty
    , testCase "no-bindist rows hidden unless installed" $ do
        let noBindist = (mkLR "9.14.1" [Latest] False False) {lNoBindist = True}
            installedNoBindist = (mkLR "9.8.4" [] True False) {lNoBindist = True}
        versionsOf ghc (curate (listingsFor ghc [noBindist, installedNoBindist]))
          @?= [lVer installedNoBindist]
    , testGroup
        "version families"
        [ testCase "familyKey is (cross, major, minor)" $
            familyKey (mkLR "9.12.2" [] False False) @?= Just (Nothing, 9, 12)
        , testCase "a cross build is its own family" $ do
            let cross = (mkLR "9.12.2" [] False False) {lCross = Just "aarch64-unknown-linux"}
            familyKey cross @?= Just (Just "aarch64-unknown-linux", 9, 12)
        , testCase "latestPerFamily keeps the newest of each family" $ do
            let newestMinor = mkLR "9.12.2" [] False False
                rows =
                  [ newestMinor
                  , mkLR "9.12.1" [] False False
                  , mkLR "9.10.1" [] False False
                  ]
                newest = latestPerFamily (Vector.fromList rows)
            Map.lookup (Nothing, 9, 12) newest @?= Just (lVer newestMinor)
            Map.lookup (Nothing, 9, 10) newest @?= Just (lVer (rows !! 2))
        , testCase "latestPerFamily does not depend on input order" $ do
            let rows = [mkLR "9.12.1" [] False False, mkLR "9.12.2" [] False False]
            latestPerFamily (Vector.fromList rows)
              @?= latestPerFamily (Vector.fromList (reverse rows))
        , testCase "isLatestInFamily marks only the newest patch" $ do
            let newer = mkLR "9.12.2" [] False False
                older = mkLR "9.12.1" [] False False
                newest = latestPerFamily (Vector.fromList [newer, older])
            isLatestInFamily newest newer @? "9.12.2 is latest in 9.12"
            not (isLatestInFamily newest older) @? "9.12.1 is not"
        , testCase "a cross build never masks the native release" $ do
            let native = mkLR "9.12.1" [] False False
                cross = (mkLR "9.12.2" [] False False) {lCross = Just "aarch64-unknown-linux"}
                newest = latestPerFamily (Vector.fromList [native, cross])
            isLatestInFamily newest native @? "native 9.12.1 is latest of its family"
            isLatestInFamily newest cross @? "cross 9.12.2 is latest of its family"
        ]
    ]
