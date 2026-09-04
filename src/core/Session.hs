module Session
  ( Model (..)
  , Phase (..)
  , PathModel (..)
  , Event (..)
  , Effect (..)
  , initialModel
  , step
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import GHCup.Types (Tool)

import Config (Config (..), ConfigUpdate (..), Filters, TableSort, applyUpdate)
import Presentation.Path (BannerSpec, appliedBanner, pathBanner)
import Presentation.Row (ToolRows, jobTitle, planRows)
import Toolchain.Path (FileChange, PathStatus (..))
import Toolchain.Types
  ( GhcupDirs
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
  | SetSensitive Bool
  | SwitchPage Phase
  | RevealStaleBanner Bool
  | Toast Text
  | ErrorToast OpError
  | Rerender (Map Tool ToolRows)
  | SaveConfig Config
  | SwitchRenderer (Map Tool ToolRows) Config
  | SetTableState TableSort Filters
  | SetListState Filters
  | CheckPath
  | ApplyPathFix (Vector FileChange)
  | SetPathBanner (Maybe BannerSpec)
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
    }

bannerFor :: Model -> Maybe BannerSpec
bannerFor model = case model.pathModel of
  Unchecked -> Nothing
  Checked status -> pathBanner model.ghcupDirs status
  FixApplied -> Just appliedBanner

-- | The row plan for the model's current listings.
rowPlan :: Model -> Map Tool ToolRows
rowPlan model = planRows model.inFlight model.listings

rerender :: Model -> Effect
rerender model = Rerender (rowPlan model)

tableState :: Model -> Effect
tableState model = SetTableState model.config.tableSort model.config.tableFilters

step :: Event -> Model -> (Model, [Effect])
step event model =
  let (model', effects) = apply event model
      sensitivity =
        [ SetSensitive (Map.null model'.inFlight)
        | Map.null model'.inFlight /= Map.null model.inFlight
        ]
  in (model', effects <> sensitivity)

apply :: Event -> Model -> (Model, [Effect])
apply event model = case event of
  Submitted (Mutate mutation)
    | Map.member (keyOfMutation mutation) model.inFlight -> (model, [])
  Submitted job@(Mutate mutation) ->
    let key = keyOfMutation mutation
        model' = model {inFlight = Map.insert key (Progress "" Nothing) model.inFlight}
    in (model', [Hold, Enqueue job, rerender model'])
  Submitted job -> (model, [Enqueue job])
  RetryClicked ->
    (model {phase = Loading}, [SwitchPage Loading, Enqueue RefreshListings])
  ConfigChanged update ->
    let model' = model {config = applyUpdate update model.config}
        echoesCurrentConfig = model'.config == model.config
        redraw = case update of
          SetAdvancedInterface _ -> [SwitchRenderer (rowPlan model') model'.config]
          SetTableSort _ -> [tableState model']
          SetTableFilters _ -> [tableState model']
          SetListFilters _ -> [SetListState model'.config.listFilters]
          SetWindowSize _ _ -> []
    in if echoesCurrentConfig
         then (model, [])
         else (model', SaveConfig model'.config : redraw)
  PathChecked status ->
    let model' = model {pathModel = Checked status}
    in (model', [SetPathBanner (bannerFor model')])
  PathFixConfirmed -> case model.pathModel of
    Checked (NeedsFixPlanned changes) -> (model, [ApplyPathFix changes])
    _ -> (model, [])
  PathFixDone (Right ()) ->
    let model' = model {pathModel = FixApplied}
    in (model', [SetPathBanner (bannerFor model')])
  PathFixDone (Left err) -> (model, [ErrorToast err])
  WorkerMsg msg -> case msg of
    ListingsReady listings stale ->
      let model' = model {listings, phase = Ready}
      in ( model'
         ,
           [ rerender model'
           , RevealStaleBanner stale
           , SwitchPage Ready
           ]
         )
    ListingsFailed err -> case model.phase of
      Ready ->
        ( model
        , [RevealStaleBanner True, ErrorToast err]
        )
      _ -> (model {phase = Offline}, [SwitchPage Offline])
    JobProgress (Mutate mutation) progress
      | key <- keyOfMutation mutation
      , Map.member key model.inFlight ->
          let model' = model {inFlight = Map.insert key progress model.inFlight}
          in (model', [rerender model'])
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
      in (model', release <> (rerender model' : outcome))
