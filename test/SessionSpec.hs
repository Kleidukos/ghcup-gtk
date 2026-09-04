module SessionSpec (tests) where

import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

import Config
  ( Config (..)
  , ConfigUpdate (..)
  , SortColumn (ByReleased)
  , SortDirection (Ascending)
  , TableFilters (..)
  , TableSort (..)
  , ViewMode (Advanced)
  , defaultConfig
  )
import Fixtures (anError, dirs, installJob, installMutation, listingsFor, lr914, sampleChanges)
import Presentation.Path (appliedBanner, pathBanner)
import Presentation.Row (planRows)
import Session
import Toolchain.Curation (CurationMode (..))
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
        [ testCase "mutation takes a hold, enqueues, dims the lists" $ do
            let (model, effects) = step (Submitted installJob) model0
            effects @?= [Hold, Enqueue installJob, SetSensitive False]
            model.inFlight @?= Set.singleton installKey
        , testCase "non-mutation only enqueues" $
            step (Submitted RefreshListings) model0
              @?= (model0, [Enqueue RefreshListings])
        ]
    , testGroup
        "listings"
        [ testCase "ready: rerender, banner, list page" $ do
            let (model, effects) = step (WorkerMsg (ListingsReady sampleListings False)) model0
            effects
              @?= [ Rerender (planRows (Curated False) sampleListings)
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
        [ testCase "progress routes to the row of the job" $
            step (WorkerMsg (JobProgress installJob (Progress "unpacking"))) model0
              @?= (model0, [SetBusy installKey (Progress "unpacking")])
        , testCase "success: release, idle the row, toast, PATH re-check, re-sensitize" $ do
            let (held, _) = step (Submitted installJob) model0
                (model, effects) = step (WorkerMsg (JobDone installMutation (Right ()))) held
            effects
              @?= [ Release
                  , SetIdle installKey
                  , Toast "GHC 9.14.1 installed"
                  , CheckPath
                  , SetSensitive True
                  ]
            model.inFlight @?= Set.empty
        , testCase "failure: release, idle, rerender, toast, re-sensitize" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (held, _) = step (Submitted installJob) ready
                (model, effects) = step (WorkerMsg (JobDone installMutation (Left anError))) held
            effects
              @?= [ Release
                  , SetIdle installKey
                  , Rerender (planRows (Curated False) sampleListings)
                  , ErrorToast anError
                  , SetSensitive True
                  ]
            model.inFlight @?= Set.empty
        , testCase "a spurious second JobDone never releases a second hold" $ do
            let (_, effects) =
                  run
                    [ Submitted installJob
                    , WorkerMsg (JobDone installMutation (Right ()))
                    , WorkerMsg (JobDone installMutation (Right ()))
                    ]
            length (filter (== Release) effects) @?= 1
        , testCase "env failure during a mutation releases exactly once" $ do
            let (model, effects) =
                  run
                    [ Submitted installJob
                    , WorkerMsg (ListingsFailed anError)
                    , WorkerMsg (JobDone installMutation (Left anError))
                    ]
            length (filter (== Release) effects) @?= 1
            model.inFlight @?= Set.empty
            model.phase @?= Offline
        ]
    , testGroup
        "retry and config"
        [ testCase "retry returns to the loading page and refetches" $ do
            let (offline, _) = step (WorkerMsg (ListingsFailed anError)) model0
                (model, effects) = step RetryClicked offline
            effects @?= [SwitchPage Loading, Enqueue RefreshListings]
            model.phase @?= Loading
        , testCase "show-old-versions saves and rerenders with the new flag" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                newConfig = defaultConfig {showOldVersions = True}
                (model, effects) = step (ConfigChanged (SetShowOldVersions True)) ready
            effects @?= [SaveConfig newConfig, Rerender (planRows (Curated True) sampleListings)]
            model.config @?= newConfig
        , testCase "sequential config updates all apply" $ do
            let (model, _) =
                  run
                    [ ConfigChanged (SetShowOldVersions True)
                    , ConfigChanged (SetShowOldVersions False)
                    ]
            model.config @?= defaultConfig
        , testCase "advanced interface: save, then switch renderer with the Full plan" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                newConfig = defaultConfig {advancedInterface = True}
                (model, effects) = step (ConfigChanged (SetAdvancedInterface True)) ready
            effects
              @?= [ SaveConfig newConfig
                  , SwitchRenderer Advanced (planRows Full sampleListings)
                  ]
            model.config @?= newConfig
        , testCase "show-old-versions while advanced still saves, and the plan is unchanged" $ do
            let advanced = model0 {config = defaultConfig {advancedInterface = True}}
                (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) advanced
                (_, effects) = step (ConfigChanged (SetShowOldVersions True)) ready
            effects
              @?= [ SaveConfig (defaultConfig {advancedInterface = True, showOldVersions = True})
                  , Rerender (planRows Full sampleListings)
                  ]
        , testCase "table sort and filters save and fan out to every table" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                sort = TableSort ByReleased Ascending
                (model, effects) = step (ConfigChanged (SetTableSort sort)) ready
            effects
              @?= [ SaveConfig defaultConfig {tableSort = sort}
                  , SetTableState sort defaultConfig.tableFilters
                  ]
            model.config.tableSort @?= sort
            let filters = TableFilters True False
                (_, filterEffects) = step (ConfigChanged (SetTableFilters filters)) ready
            filterEffects
              @?= [ SaveConfig defaultConfig {tableFilters = filters}
                  , SetTableState defaultConfig.tableSort filters
                  ]
        , testCase "table state never re-plans: GTK has already applied it" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
                (_, effects) =
                  step (ConfigChanged (SetTableFilters (TableFilters True True))) ready
            filter isRerender effects @?= []
        , testCase "an echoed config update emits nothing, which is what stops the sort-save-apply-sort loop" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings False)) model0
            snd (step (ConfigChanged (SetTableSort defaultConfig.tableSort)) ready) @?= []
            snd (step (ConfigChanged (SetShowOldVersions False)) ready) @?= []
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
        , testCase "a stray confirmation without a fixable status is a no-op" $ do
            let (checked, _) = step (PathChecked PathOk) model0
            step PathFixConfirmed checked @?= (checked, [])
            step PathFixConfirmed model0 @?= (model0, [])
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
