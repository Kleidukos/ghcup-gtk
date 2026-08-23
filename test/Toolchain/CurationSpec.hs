module Toolchain.CurationSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Versions (Version)
import Test.Tasty
import Test.Tasty.HUnit

import Fixtures (listingsFor, mkLR)
import GHCup.Command.List (ListResult (..))
import GHCup.Types (Tag (..))
import Toolchain.Curation
import Toolchain.Types (Listings, SupportedTool (..))

versionsOf :: SupportedTool -> Listings -> [Version]
versionsOf tool listings =
  map lVer (Vector.toList (Map.findWithDefault Vector.empty tool listings))

tests :: TestTree
tests =
  testGroup
    "Curation"
    [ testCase "keeps recommended, latest, installed; hides the rest" $ do
        let rows =
              [ mkLR "9.10.2" [Recommended] False False
              , mkLR "9.14.1" [Latest] False False
              , mkLR "9.8.4" [] True True
              , mkLR "8.10.7" [] False False
              ]
        versionsOf GHC (curate False (listingsFor GHC rows))
          @?= map lVer [rows !! 1, head rows, rows !! 2]
    , testCase "toggle reveals everything" $ do
        let rows =
              [ mkLR "9.14.1" [Latest] False False
              , mkLR "8.10.7" [] False False
              ]
        length (versionsOf GHC (curate True (listingsFor GHC rows))) @?= 2
    , testCase "sorts descending by version" $ do
        let rows =
              [ mkLR "3.10.3.0" [] True False
              , mkLR "3.14.1.0" [Latest] False False
              ]
        versionsOf Cabal (curate False (listingsFor Cabal rows))
          @?= map lVer [rows !! 1, head rows]
    , testCase "tools stay separate" $ do
        let listings =
              Map.fromList
                [ (GHC, Vector.fromList [mkLR "9.14.1" [Latest] False False])
                , (Cabal, Vector.fromList [mkLR "3.14.1.0" [Latest] False False])
                ]
        Map.keys (curate False listings) @?= Map.keys listings
    , testCase "empty input, empty output" $
        curate False Map.empty @?= Map.empty
    , testCase "no-bindist rows hidden unless installed, even with toggle on" $ do
        let noBindist = (mkLR "9.14.1" [Latest] False False){lNoBindist = True}
            installedNoBindist = (mkLR "9.8.4" [] True False){lNoBindist = True}
        versionsOf GHC (curate True (listingsFor GHC [noBindist, installedNoBindist]))
          @?= [lVer installedNoBindist]
    ]
