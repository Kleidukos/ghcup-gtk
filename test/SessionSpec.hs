module SessionSpec (tests) where

import Data.Map.Strict qualified as Map
import GHCup.Types (cabal, ghc)
import Test.Tasty
import Test.Tasty.HUnit

import Config
  ( Config (..)
  , ConfigUpdate (..)
  , defaultConfig
  )
import Fixtures (anError, dirs, installJob, installMutation, listingsFor, lr914, sampleChanges)
import Presentation.Path (appliedBanner, pathBanner)
import Presentation.Row (planRows)
import Session
import Toolchain.Path (PathStatus (..))
import Toolchain.Types

sampleListings :: Listings
sampleListings = listingsFor ghc [lr914]

installKey :: RowKey
installKey = keyOfListing ghc lr914

model0 :: Model
model0 = initialModel dirs defaultConfig

tests :: TestTree
tests =
  testGroup
    "Session"
    [ testGroup
        "RowKey"
        [ testCase "job-side and listings-side keys agree for every mutation" $ do
            keyOfMutation (Install ghc (reqOf lr914) defaultInstallOptions) @?= installKey
            keyOfMutation (Uninstall ghc (tvOf lr914)) @?= installKey
            keyOfMutation (SetDefault ghc (tvOf lr914)) @?= installKey
        , testCase "rowKeyText is stable and distinguishes tool and version" $ do
            rowKeyText installKey @?= "ghc:9.14.1"
            rowKeyText (keyOfListing cabal lr914) @?= "cabal:9.14.1"
        ]
    , testGroup
        "Submitted"
        [ testCase "mutation takes a hold, enqueues, stamps the row, dims the lists" $ do
            let (model, effects) = step (Submitted installJob) model0
            effects @?= [Hold, Enqueue installJob, Reconcile]
            model.inFlight @?= Map.singleton installKey (Progress "" Nothing)
            model.listings @?= mempty
        , testCase "non-mutation only enqueues" $
            step (Submitted RefreshListings) model0
              @?= (model0, [Enqueue RefreshListings])
        , testCase "a second submit for a row already in flight is dropped" $ do
            let (held, _) = step (Submitted installJob) model0
                (model, effects) = step (Submitted installJob) held
            effects @?= []
            model.inFlight @?= Map.singleton installKey (Progress "" Nothing)
        ]
    , testGroup
        "listings"
        [ testCase "ready: reconcile with the fresh listings in the model" $ do
            let (model, effects) = step (WorkerMsg (ListingsReady sampleListings False)) model0
            effects @?= [Reconcile]
            model.phase @?= Ready
            model.stale @?= False
            rowPlan model @?= planRows Map.empty sampleListings
        , testCase "failure before anything loaded lands on the offline page" $ do
            let (model, effects) = step (WorkerMsg (ListingsFailed anError)) model0
            effects @?= [Reconcile]
            model.phase @?= Offline
        , testCase "failure after Ready degrades to staleness + toast, a fresh success clears it" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (staleModel, effects) = step (WorkerMsg (ListingsFailed anError)) ready
            effects @?= [Reconcile, ErrorToast anError]
            staleModel.phase @?= Ready
            staleModel.stale @?= True
            let (model, _) = step (WorkerMsg (ListingsReady sampleListings False)) staleModel
            model.stale @?= False
        , testCase "a stale-flagged delivery stamps staleness" $
            (fst (step (WorkerMsg (ListingsReady sampleListings True)) model0)).stale @?= True
        ]
    , testGroup
        "jobs"
        [ testCase "progress stamps the model and reconciles" $ do
            let (held, _) = step (Submitted installJob) model0
                (model, effects) = step (WorkerMsg (JobProgress installJob (Progress "unpacking" Nothing))) held
            model.inFlight @?= Map.singleton installKey (Progress "unpacking" Nothing)
            effects @?= [Reconcile]
        , testCase "progress for an untracked job is ignored" $
            step (WorkerMsg (JobProgress installJob (Progress "unpacking" Nothing))) model0
              @?= (model0, [])
        , testCase "progress for a refresh is ignored" $
            step (WorkerMsg (JobProgress RefreshListings (Progress "fetching" Nothing))) model0
              @?= (model0, [])
        , testCase "success: release, reconcile without the stamp, toast, PATH re-check" $ do
            let (held, _) = step (Submitted installJob) model0
                (model, effects) = step (WorkerMsg (JobDone installMutation (Right ()))) held
            effects
              @?= [ Release
                  , Reconcile
                  , Toast "GHC 9.14.1 installed"
                  , CheckPath
                  ]
            model.inFlight @?= Map.empty
        , testCase "failure: release, reconcile without the stamp, toast" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (held, _) = step (Submitted installJob) ready
                (model, effects) = step (WorkerMsg (JobDone installMutation (Left anError))) held
            effects
              @?= [ Release
                  , Reconcile
                  , ErrorToast anError
                  ]
            model.inFlight @?= Map.empty
            rowPlan model @?= planRows Map.empty sampleListings
        ]
    , testGroup
        "retry and config"
        [ testCase "retry returns to the loading page and refetches" $ do
            let (offline, _) = step (WorkerMsg (ListingsFailed anError)) model0
                (model, effects) = step RetryClicked offline
            effects @?= [Reconcile, Enqueue RefreshListings]
            model.phase @?= Loading
        , testCase "advanced interface: save, then reconcile with the new config in the model" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                newConfig = defaultConfig {advancedInterface = True}
                (model, effects) = step (ConfigChanged (SetAdvancedInterface True)) ready
            effects @?= [SaveConfig newConfig, Reconcile]
            model.config @?= newConfig
        , testCase "a window resize saves without reconciling" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (_, effects) = step (ConfigChanged (SetWindowSize 1000 700)) ready
            filter (== Reconcile) effects @?= []
        , testCase "an echoed config update emits nothing, which is what stops the sort-save-apply-sort loop" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
            snd (step (ConfigChanged (SetTableSort defaultConfig.tableSort)) ready) @?= []
            snd (step (ConfigChanged (SetListFilters defaultConfig.listFilters)) ready) @?= []
            snd (step (ConfigChanged (SetAdvancedInterface False)) ready) @?= []
        ]
    , testGroup
        "PATH fix"
        [ testCase "PathOk: no banner" $ do
            let (model, effects) = step (PathChecked PathOk) model0
            effects @?= [Reconcile]
            model.pathModel @?= Checked PathOk
            bannerFor model @?= Nothing
        , testCase "a fixable status renders the offer banner" $ do
            let status = NeedsFixPlanned sampleChanges
                (model, effects) = step (PathChecked status) model0
            effects @?= [Reconcile]
            model.pathModel @?= Checked status
            bannerFor model @?= pathBanner dirs status
        , testCase "confirming the fix applies the checked plan" $ do
            let (checked, _) = step (PathChecked (NeedsFixPlanned sampleChanges)) model0
            step PathFixConfirmed checked
              @?= (checked, [ApplyPathFix sampleChanges])
        , testCase "a successful fix shows the applied banner, a later re-check clears it" $ do
            let (checked, _) = step (PathChecked (NeedsFixPlanned sampleChanges)) model0
                (applied, effects) = step (PathFixDone (Right ())) checked
            effects @?= [Reconcile]
            applied.pathModel @?= FixApplied
            bannerFor applied @?= Just appliedBanner
            let (model, _) = step (PathChecked PathOk) applied
            bannerFor model @?= Nothing
        , testCase "a failed fix toasts and keeps offering" $ do
            let (checked, _) = step (PathChecked (NeedsFixPlanned sampleChanges)) model0
                (model, effects) = step (PathFixDone (Left anError)) checked
            effects @?= [ErrorToast anError]
            model.pathModel @?= Checked (NeedsFixPlanned sampleChanges)
        ]
    ]
