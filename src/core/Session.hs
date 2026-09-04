module Session
  ( Model (..)
  , Phase (..)
  , PathModel (..)
  , Event (..)
  , Effect (..)
  , initialModel
  , bannerFor
  , rowPlan
  , step
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import GHCup.Types (Tool)

import Config (Config (..), ConfigUpdate (..), applyUpdate)
import Presentation.Path (BannerSpec, appliedBanner, pathBanner)
import Presentation.Row (ToolRows, jobTitle, planRows)
import Toolchain.Path (FileChange, PathStatus (..))
import Toolchain.Types
  ( Freshness (..)
  , GhcupDirs
  , Job (..)
  , Listings
  , OpError
  , Progress (Progress)
  , RowKey
  , UiMsg (..)
  , keyOfMutation
  )

data Phase = Loading | Offline | Ready
  deriving stock (Eq, Show)

-- | UI phase of the PATH story. 'PathStatus' is what a check finds;
-- 'FixApplied' exists only here, after the user applied the fix.
data PathModel
  = Unchecked
  | Checked PathStatus
  | FixApplied
  deriving stock (Eq, Show)

data Model = Model
  { listings :: Listings
  -- ^ Version data for each tool
  , config :: Config
  -- ^ User preferences
  , phase :: Phase
  -- ^ Top-level page currently being shown
  , inFlight :: Map RowKey Progress
  -- ^ Mutations still running, with the latest progress report of each
  , ghcupDirs :: GhcupDirs
  -- ^ ghcup's directories
  , pathModel :: PathModel
  -- ^ Status of the PATH fixing
  , freshness :: Freshness
  -- ^ Whether the listings reflect current toolchain metadata
  }
  deriving stock (Eq, Show)

data Event
  = -- |  The user asks for an action
    Submitted Job
  | -- |  Messages reported by the worker
    WorkerMsg UiMsg
  | -- | User clicks the "retry" button
    RetryClicked
  | -- | Configuration has changed
    ConfigChanged ConfigUpdate
  | -- | A PATH check finished
    PathChecked PathStatus
  | -- | User confirmed the PATH-fix dialog
    PathFixConfirmed
  | -- | The PATH fix was applied (or failed)
    PathFixDone (Either OpError ())
  deriving stock (Eq, Show)

data Effect
  = Enqueue Job
  | Hold
  | Release
  | Toast Text
  | ErrorToast OpError
  | Reconcile
  | SaveConfig Config
  | CheckPath
  | ApplyPathFix (Vector FileChange)
  deriving stock (Eq, Show)

initialModel :: GhcupDirs -> Config -> Model
initialModel ghcupDirs config =
  Model
    { listings = mempty
    , config
    , phase = Loading
    , inFlight = Map.empty
    , ghcupDirs
    , pathModel = Unchecked
    , freshness = Fresh
    }

bannerFor :: Model -> Maybe BannerSpec
bannerFor model = case model.pathModel of
  Unchecked -> Nothing
  Checked status -> pathBanner model.ghcupDirs status
  FixApplied -> Just appliedBanner

-- | The row plan for the model's current listings.
rowPlan :: Model -> Map Tool ToolRows
rowPlan model = planRows model.inFlight model.listings

step :: Event -> Model -> (Model, [Effect])
step event model = case event of
  Submitted (Mutate mutation)
    | Map.member (keyOfMutation mutation) model.inFlight -> (model, [])
  Submitted job@(Mutate mutation) ->
    let key = keyOfMutation mutation
        model' = model {inFlight = Map.insert key (Progress "" Nothing) model.inFlight}
    in (model', [Hold, Enqueue job, Reconcile])
  Submitted job -> (model, [Enqueue job])
  RetryClicked ->
    (model {phase = Loading}, [Reconcile, Enqueue RefreshListings])
  ConfigChanged update ->
    let model' = model {config = applyUpdate update model.config}
        echoesCurrentConfig = model'.config == model.config
        redraw = case update of
          SetWindowSize _ _ -> []
          _ -> [Reconcile]
    in if echoesCurrentConfig
         then (model, [])
         else (model', SaveConfig model'.config : redraw)
  PathChecked status ->
    (model {pathModel = Checked status}, [Reconcile])
  PathFixConfirmed -> case model.pathModel of
    Checked (NeedsFixPlanned changes) -> (model, [ApplyPathFix changes])
    _ -> (model, [])
  PathFixDone (Right ()) ->
    (model {pathModel = FixApplied}, [Reconcile])
  PathFixDone (Left err) -> (model, [ErrorToast err])
  WorkerMsg msg -> case msg of
    ListingsReady listings freshness ->
      (model {listings, phase = Ready, freshness}, [Reconcile])
    Relisted listings ->
      (model {listings, phase = Ready}, [Reconcile])
    ListingsFailed err -> case model.phase of
      Ready -> (model {freshness = Stale}, [Reconcile, ErrorToast err])
      _ -> (model {phase = Offline}, [Reconcile])
    JobProgress (Mutate mutation) progress
      | key <- keyOfMutation mutation
      , Map.member key model.inFlight ->
          let model' = model {inFlight = Map.insert key progress model.inFlight}
          in (model', [Reconcile])
    JobProgress _ _ -> (model, [])
    JobDone mutation result ->
      let key = keyOfMutation mutation
          (model', release)
            | Map.member key model.inFlight =
                (model {inFlight = Map.delete key model.inFlight}, [Release])
            | otherwise = (model, [])
          outcome = case result of
            Right () -> [Toast (jobTitle mutation), CheckPath]
            Left err -> [ErrorToast err]
      in (model', release <> (Reconcile : outcome))
