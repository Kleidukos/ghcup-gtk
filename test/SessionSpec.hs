module SessionSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHCup.Types (ChannelAlias (..), NewURLSource (..), cabal, ghc, hls)
import Test.Tasty
import Test.Tasty.HUnit

import Config
  ( Config (..)
  , ConfigUpdate (..)
  , defaultConfig
  )
import Fixtures (anError, defaultCompileGhcOptions, defaultCompileHlsOptions, defaultNightliesUri, dirs, installJob, installMutation, listingsFor, lr914, sampleChanges)
import Presentation.Filter (Channel (..))
import Presentation.Path (appliedBanner, pathBanner)
import Presentation.Row (Confirmation (..), RowAction (..), planRows)
import Session
import Toolchain.Channels (defaultNightliesUrl)
import Toolchain.Path (PathStatus (..))
import Toolchain.Types

sampleListings :: Listings
sampleListings = listingsFor ghc [lr914]

installKey :: RowKey
installKey = keyOfListing ghc lr914

model0 :: Model
model0 = initialModel dirs defaultConfig [NewGHCupURL] ChannelsEditable

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
            keyOfMutation (CompileGhc (tvOf lr914) (defaultCompileGhcOptions (Right "/usr/bin/ghc"))) @?= installKey
            keyOfMutation (CompileHls (tvOf lr914) (defaultCompileHlsOptions [])) @?= keyOfListing hls lr914
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
        , testCase "a confirm request becomes a Confirm effect carrying the job, model untouched" $ do
            let action = RowAction "Remove" (Confirmation "h" "b" "Remove" True) (Uninstall ghc (tvOf lr914))
            step (ConfirmRequested action) model0
              @?= (model0, [Confirm action.confirmation (Mutate action.job)])
        ]
    , testGroup
        "listings"
        [ testCase "ready: reconcile with the fresh listings in the model" $ do
            let (model, effects) = step (WorkerMsg (ListingsReady sampleListings Fresh)) model0
            effects @?= [Reconcile]
            model.phase @?= Ready
            model.freshness @?= Fresh
            rowPlan model @?= planRows Map.empty sampleListings
        , testCase "failure before anything loaded lands on the offline page" $ do
            let (model, effects) = step (WorkerMsg (ListingsFailed anError)) model0
            effects @?= [Reconcile]
            model.phase @?= Offline
        , testCase "failure after Ready degrades to staleness + toast, a fresh success clears it" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings Fresh)) model0
                (staleModel, effects) = step (WorkerMsg (ListingsFailed anError)) ready
            effects @?= [Reconcile, ErrorToast anError]
            staleModel.phase @?= Ready
            staleModel.freshness @?= Stale
            let (model, _) = step (WorkerMsg (ListingsReady sampleListings Fresh)) staleModel
            model.freshness @?= Fresh
        , testCase "a stale-flagged delivery stamps staleness" $
            (fst (step (WorkerMsg (ListingsReady sampleListings Stale)) model0)).freshness @?= Stale
        , testCase "a relist swaps the listings and keeps the freshness" $ do
            let (staleReady, _) = step (WorkerMsg (ListingsReady sampleListings Stale)) model0
                (model, effects) = step (WorkerMsg (Relisted sampleListings)) staleReady
            effects @?= [Reconcile]
            model.phase @?= Ready
            model.freshness @?= Stale
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
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings Fresh)) model0
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
        , testCase "a window resize saves the new config in the model, without reconciling" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings Fresh)) model0
                newConfig = defaultConfig {windowWidth = 1000, windowHeight = 700}
                (model, effects) = step (ConfigChanged (SetWindowSize 1000 700)) ready
            effects @?= [SaveConfig newConfig]
            model.config @?= newConfig
        , testCase "an echoed config update emits nothing, which is what stops a save loop" $ do
            let (ready, _) = step (WorkerMsg (ListingsReady sampleListings Fresh)) model0
            snd (step (ConfigChanged (SetWindowSize defaultConfig.windowWidth defaultConfig.windowHeight)) ready) @?= []
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
    , testGroup
        "channels"
        [ testCase "a change asks for the requested set to be persisted, without touching the model yet" $ do
            let (model, effects) = step (ChannelsChanged (Set.singleton Prereleases) Nothing) model0
            model @?= model0
            effects @?= [PersistChannels (Set.singleton Prereleases) Nothing]
        , testCase "a no-op change is dropped" $
            step (ChannelsChanged Set.empty Nothing) model0 @?= (model0, [])
        , testCase "a change against a locked model is dropped" $ do
            let locked = initialModel dirs defaultConfig [NewGHCupURL] ChannelsLocked
            step (ChannelsChanged (Set.singleton Prereleases) Nothing) locked @?= (locked, [])
        , testCase "a successful save updates the model and reconfigures the worker" $ do
            let sources = [NewGHCupURL, NewChannelAlias CrossChannel]
                (model, effects) = step (ChannelsSaved sources) model0
            model.urlSource @?= sources
            effects @?= [Enqueue (Reconfigure sources), Reconcile]
        , testCase "a save carrying a nightlies URI remembers it in the app config" $ do
            let sources = [NewGHCupURL, NewURI defaultNightliesUri]
                (model, effects) = step (ChannelsSaved sources) model0
                config' = defaultConfig {nightliesUrl = Just defaultNightliesUrl}
            model.config @?= config'
            effects @?= [SaveConfig config', Enqueue (Reconfigure sources), Reconcile]
        , testCase "a save without a nightlies URI leaves the config alone" $ do
            let sources = [NewGHCupURL, NewChannelAlias CrossChannel]
                (model, effects) = step (ChannelsSaved sources) model0
            model.config @?= defaultConfig
            effects @?= [Enqueue (Reconfigure sources), Reconcile]
        , testCase "re-saving the same nightlies URI does not save the config again" $ do
            let sources = [NewGHCupURL, NewURI defaultNightliesUri]
                (remembered, _) = step (ChannelsSaved sources) model0
                (model, effects) = step (ChannelsSaved sources) remembered
            model.config @?= remembered.config
            effects @?= [Enqueue (Reconfigure sources), Reconcile]
        , testCase "a disabled nightlies channel keeps the remembered URL" $ do
            let (remembered, _) = step (ChannelsSaved [NewGHCupURL, NewURI defaultNightliesUri]) model0
                (model, effects) = step (ChannelsSaved [NewGHCupURL]) remembered
            model.config.nightliesUrl @?= Just defaultNightliesUrl
            effects @?= [Enqueue (Reconfigure [NewGHCupURL]), Reconcile]
        ]
    ]
