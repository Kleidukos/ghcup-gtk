module SessionSpec (tests) where

import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit

import Config
  ( Config (..)
  , ConfigUpdate (..)
  , Filters (..)
  , SortColumn (ByReleased)
  , SortDirection (Ascending)
  , TableSort (..)
  , defaultConfig
  )
import Fixtures (anError, dirs, installJob, installMutation, listingsFor, lr914, sampleChanges)
import Presentation.Path (appliedBanner, pathBanner)
import Presentation.Row (planRows)
import Session
import Toolchain.Path (PathStatus (..))
import Toolchain.Types

sampleListings :: Listings
sampleListings = listingsFor GHC [lr914]

installKey :: RowKey
installKey = keyOfListing GHC lr914

model0 :: Model
model0 = initialModel dirs defaultConfig

run :: [Event] -> (Model, [Effect])
run = foldl' go (model0, [])
  where
    go (model, effects) event =
      let (model', new) = step event model
      in (model', effects <> new)

tests :: TestTree
tests =
  testGroup
    "Session"
    [ testGroup
        "RowKey"
        [ testCase "job-side and listings-side keys agree for every mutation" $ do
            keyOfMutation (Install GHC (reqOf lr914)) @?= installKey
            keyOfMutation (Uninstall GHC (tvOf lr914)) @?= installKey
            keyOfMutation (SetDefault GHC (tvOf lr914)) @?= installKey
        , testCase "rowKeyText is stable and distinguishes tool and version" $ do
            rowKeyText installKey @?= "GHC:9.14.1"
            rowKeyText (keyOfListing Cabal lr914) @?= "Cabal:9.14.1"
        ]
    , testGroup
        "Submitted"
        [ testCase "mutation takes a hold, enqueues, stamps the row, dims the lists" $ do
            let (model, effects) = step (Submitted installJob) model0
            effects
              @?= [ Hold
                  , Enqueue installJob
                  , Rerender (planRows (Map.singleton installKey (Progress "" Nothing)) mempty)
                  , SetSensitive False
                  ]
            model.inFlight @?= Map.singleton installKey (Progress "" Nothing)
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
        [ testCase "ready: rerender, banner, list page" $ do
            let (model, effects) = step (WorkerMsg (ListingsReady sampleListings False)) model0
            effects
              @?= [ Rerender (planRows Map.empty sampleListings)
                  , RevealStaleBanner False
                  , SwitchPage Ready
                  ]
            model.phase @?= Ready
        , testCase "failure before anything loaded lands on the offline page" $ do
            let (model, effects) = step (WorkerMsg (ListingsFailed anError)) model0
            effects @?= [SwitchPage Offline]
            model.phase @?= Offline
        , testCase "failure after Ready degrades to stale banner + toast" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (model, effects) = step (WorkerMsg (ListingsFailed anError)) ready
            effects @?= [RevealStaleBanner True, ErrorToast anError]
            model.phase @?= Ready
        , testCase "a fresh success clears staleness" $ do
            let (_, effects) =
                  run
                    [ WorkerMsg (ListingsReady sampleListings True)
                    , WorkerMsg (ListingsReady sampleListings False)
                    ]
            filter isBannerEffect effects
              @?= [RevealStaleBanner True, RevealStaleBanner False]
        ]
    , testGroup
        "jobs"
        [ testCase "progress stamps the model and rerenders the row" $ do
            let (held, _) = step (Submitted installJob) model0
                (model, effects) = step (WorkerMsg (JobProgress installJob (Progress "unpacking" Nothing))) held
            model.inFlight @?= Map.singleton installKey (Progress "unpacking" Nothing)
            effects
              @?= [Rerender (planRows (Map.singleton installKey (Progress "unpacking" Nothing)) mempty)]
        , testCase "progress for an untracked job is ignored" $
            step (WorkerMsg (JobProgress installJob (Progress "unpacking" Nothing))) model0
              @?= (model0, [])
        , testCase "progress for a refresh is ignored" $
            step (WorkerMsg (JobProgress RefreshListings (Progress "fetching" Nothing))) model0
              @?= (model0, [])
        , testCase "success: release, rerender without the stamp, toast, PATH re-check, re-sensitize" $ do
            let (held, _) = step (Submitted installJob) model0
                (model, effects) = step (WorkerMsg (JobDone installMutation (Right ()))) held
            effects
              @?= [ Release
                  , Rerender (planRows Map.empty mempty)
                  , Toast "GHC 9.14.1 installed"
                  , CheckPath
                  , SetSensitive True
                  ]
            model.inFlight @?= Map.empty
        , testCase "failure: release, rerender without the stamp, toast, re-sensitize" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (held, _) = step (Submitted installJob) ready
                (model, effects) = step (WorkerMsg (JobDone installMutation (Left anError))) held
            effects
              @?= [ Release
                  , Rerender (planRows Map.empty sampleListings)
                  , ErrorToast anError
                  , SetSensitive True
                  ]
            model.inFlight @?= Map.empty
        ]
    , testGroup
        "retry and config"
        [ testCase "retry returns to the loading page and refetches" $ do
            let (offline, _) = step (WorkerMsg (ListingsFailed anError)) model0
                (model, effects) = step RetryClicked offline
            effects @?= [SwitchPage Loading, Enqueue RefreshListings]
            model.phase @?= Loading
        , testCase "advanced interface: save, then switch renderer with the plan and new config" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                newConfig = defaultConfig {advancedInterface = True}
                (model, effects) = step (ConfigChanged (SetAdvancedInterface True)) ready
            effects
              @?= [ SaveConfig newConfig
                  , SwitchRenderer (planRows Map.empty sampleListings) newConfig
                  ]
            model.config @?= newConfig
        , testCase "list filters save and fan out to every list" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                filters = Filters True False
                (model, effects) = step (ConfigChanged (SetListFilters filters)) ready
            effects
              @?= [ SaveConfig defaultConfig {listFilters = filters}
                  , SetListState filters
                  ]
            model.config.listFilters @?= filters
        , testCase "table sort and filters save and fan out to every table" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                sort = TableSort ByReleased Ascending
                (model, effects) = step (ConfigChanged (SetTableSort sort)) ready
            effects
              @?= [ SaveConfig defaultConfig {tableSort = sort}
                  , SetTableState sort defaultConfig.tableFilters
                  ]
            model.config.tableSort @?= sort
            let filters = Filters True False
                (_, filterEffects) = step (ConfigChanged (SetTableFilters filters)) ready
            filterEffects
              @?= [ SaveConfig defaultConfig {tableFilters = filters}
                  , SetTableState defaultConfig.tableSort filters
                  ]
        , testCase "table and list state never re-plan: GTK has already applied them" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (_, effects) =
                  step (ConfigChanged (SetTableFilters (Filters True True))) ready
                (_, listEffects) =
                  step (ConfigChanged (SetListFilters (Filters True True))) ready
            filter isRerender effects @?= []
            filter isRerender listEffects @?= []
        , testCase "an echoed config update emits nothing, which is what stops the sort-save-apply-sort loop" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
            snd (step (ConfigChanged (SetTableSort defaultConfig.tableSort)) ready) @?= []
            snd (step (ConfigChanged (SetListFilters defaultConfig.listFilters)) ready) @?= []
            snd (step (ConfigChanged (SetAdvancedInterface False)) ready) @?= []
        ]
    , testGroup
        "PATH fix"
        [ testCase "PathOk: no banner" $
            step (PathChecked PathOk) model0
              @?= (model0 {pathModel = Checked PathOk}, [SetPathBanner Nothing])
        , testCase "a fixable status renders the offer banner" $ do
            let status = NeedsFixPlanned sampleChanges
                (model, effects) = step (PathChecked status) model0
            effects @?= [SetPathBanner (pathBanner dirs status)]
            model.pathModel @?= Checked status
        , testCase "confirming the fix applies the checked plan" $ do
            let (checked, _) = step (PathChecked (NeedsFixPlanned sampleChanges)) model0
            step PathFixConfirmed checked
              @?= (checked, [ApplyPathFix sampleChanges])
        , testCase "a successful fix shows the applied banner" $ do
            let (checked, _) = step (PathChecked (NeedsFixPlanned sampleChanges)) model0
                (model, effects) = step (PathFixDone (Right ())) checked
            effects @?= [SetPathBanner (Just appliedBanner)]
            model.pathModel @?= FixApplied
        , testCase "a failed fix toasts and keeps offering" $ do
            let (checked, _) = step (PathChecked (NeedsFixPlanned sampleChanges)) model0
                (model, effects) = step (PathFixDone (Left anError)) checked
            effects @?= [ErrorToast anError]
            model.pathModel @?= Checked (NeedsFixPlanned sampleChanges)
        , testCase "a later re-check can clear the applied state" $ do
            let (_, effects) =
                  run
                    [ PathChecked (NeedsFixPlanned sampleChanges)
                    , PathFixDone (Right ())
                    , PathChecked PathOk
                    ]
            filter isPathBanner effects
              @?= [ SetPathBanner (pathBanner dirs (NeedsFixPlanned sampleChanges))
                  , SetPathBanner (Just appliedBanner)
                  , SetPathBanner Nothing
                  ]
        ]
    ]
  where
    isBannerEffect = \case
      RevealStaleBanner _ -> True
      _ -> False
    isPathBanner = \case
      SetPathBanner _ -> True
      _ -> False
    isRerender = \case
      Rerender _ -> True
      _ -> False
