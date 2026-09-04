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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (Vector)

import Config (Config (..), ConfigUpdate, applyUpdate)
import Presentation (BannerSpec, ToolRows, appliedBanner, jobTitle, pathBanner, planRows)
import Toolchain.Path (FileChange, PathStatus (..))
import Toolchain.Types
  ( GhcupDirs
  , Job (..)
  , Listings
  , OpError
  , Progress
  , RowKey
  , SupportedTool
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
  , inFlight :: Set RowKey
  -- ^ Mutations still running
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
  | SetBusy RowKey Progress
  | SetIdle RowKey
  | Rerender (Map SupportedTool ToolRows)
  | SaveConfig Config
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
    , inFlight = Set.empty
    , ghcupDirs
    , pathModel = Unchecked
    }

bannerFor :: Model -> Maybe BannerSpec
bannerFor model = case model.pathModel of
  Unchecked -> Nothing
  Checked status -> pathBanner model.ghcupDirs status
  FixApplied -> Just appliedBanner

-- | The row plan for a model's current listings and preferences.
rerender :: Model -> Effect
rerender model = Rerender (planRows model.config.showOldVersions model.listings)

step :: Event -> Model -> (Model, [Effect])
step event model =
  let (model', effects) = apply event model
      sensitivity =
        [ SetSensitive (Set.null model'.inFlight)
        | Set.null model'.inFlight /= Set.null model.inFlight
        ]
   in (model', effects <> sensitivity)

apply :: Event -> Model -> (Model, [Effect])
apply event model = case event of
  Submitted job@(Mutate mutation)
    | key <- keyOfMutation mutation
    , not (Set.member key model.inFlight) ->
        ( model{inFlight = Set.insert key model.inFlight}
        , [Hold, Enqueue job]
        )
  Submitted job -> (model, [Enqueue job])
  RetryClicked ->
    (model{phase = Loading}, [SwitchPage Loading, Enqueue RefreshListings])
  ConfigChanged update ->
    let model' = model{config = applyUpdate update model.config}
     in (model', [SaveConfig model'.config, rerender model'])
  PathChecked status ->
    let model' = model{pathModel = Checked status}
     in (model', [SetPathBanner (bannerFor model')])
  PathFixConfirmed -> case model.pathModel of
    Checked (NeedsFixPlanned changes) -> (model, [ApplyPathFix changes])
    _ -> (model, [])
  PathFixDone (Right ()) ->
    let model' = model{pathModel = FixApplied}
     in (model', [SetPathBanner (bannerFor model')])
  PathFixDone (Left err) -> (model, [ErrorToast err])
  WorkerMsg msg -> case msg of
    ListingsReady listings stale ->
      let model' = model{listings, phase = Ready}
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
      _ -> (model{phase = Offline}, [SwitchPage Offline])
    JobProgress job progress ->
      (model, [SetBusy (keyOfMutation mutation) progress | Mutate mutation <- [job]])
    JobDone mutation result ->
      let
        key = keyOfMutation mutation
        (model', release)
          | Set.member key model.inFlight =
              (model{inFlight = Set.delete key model.inFlight}, [Release])
          | otherwise = (model, [])
        outcome = case result of
          Right () -> [Toast (jobTitle mutation), CheckPath]
          Left err ->
            [ rerender model'
            , ErrorToast err
            ]
       in
        (model', release <> (SetIdle key : outcome))
