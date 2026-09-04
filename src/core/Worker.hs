module Worker
  ( Handle
  , new
  , start
  , enqueue
  , processJob
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Exception (SomeException, try)
import Effectful.State.Static.Shared (State, evalState, get, put)
import GHC.Clock (getMonotonicTime)

import Effects.Ghcup
import Effects.Notify (Notify, emit, runNotifyIO)
import Toolchain.Types

newtype Handle = Handle
  { queue :: TQueue Job
  }

new :: IO Handle
new = Handle <$> newTQueueIO

enqueue :: Handle -> Job -> IO ()
enqueue handle job = atomically $ writeTQueue handle.queue job

start :: Handle -> (UiMsg -> IO ()) -> IO ()
start handle notify =
  void $
    forkIO $
      runEff $
        runNotifyIO notify $
          evalState RefreshListings $ -- the job progress reports attach to
            evalState (0 :: Double) $ -- when progress was last emitted
              runGhcupIO throttledProgress $
                forever $ do
                  job <- liftIO (atomically (readTQueue handle.queue))
                  processJob put job

throttledProgress
  :: (IOE :> es, Notify :> es, State Job :> es, State Double :> es)
  => Text
  -> Eff es ()
throttledProgress line = do
  now <- liftIO getMonotonicTime
  before <- get
  when (now - before >= 0.1) $ do
    put now
    job <- get @Job
    emit (JobProgress job (progressOf line))

processJob
  :: (Ghcup :> es, Notify :> es)
  => (Job -> Eff es ())
  -> Job
  -> Eff es ()
processJob setCurrent job = do
  setCurrent job
  try @SomeException (runJob setCurrent job) >>= \case
    Right () -> pure ()
    Left e ->
      let err = OpError "Unexpected error" (Text.pack (show e))
      in emit $ case job of
           Mutate mutation -> JobDone mutation (Left err)
           RefreshListings -> ListingsFailed err

runJob :: (Ghcup :> es, Notify :> es) => (Job -> Eff es ()) -> Job -> Eff es ()
runJob setCurrent = \case
  RefreshListings -> emitListings =<< fetchListings
  Mutate mutation -> do
    result <- runMutation mutation
    emit (JobDone mutation result)
    relistAfterMutation
  where
    emitListings = \case
      Right (listings, stale) -> emit (ListingsReady listings stale)
      Left err -> emit (ListingsFailed err)

    runMutation = \case
      Install tool tvr opts -> installTool tool tvr opts
      Uninstall tool tv -> uninstallTool tool tv
      SetDefault tool tv -> setDefaultVersion tool tv
      CompileGhc tv opts -> compileGhcTool tv opts
      CompileHls tv opts -> compileHlsTool tv opts

    relistAfterMutation = do
      setCurrent RefreshListings
      emitListings =<< (attempt relistListings `fallbackTo` attempt fetchListings)

-- | Fold an exception into the operation's own error channel.
attempt :: Eff es (Either OpError a) -> Eff es (Either OpError a)
attempt op =
  try @SomeException op >>= \case
    Right result -> pure result
    Left e -> pure (Left (OpError "Could not refresh listings" (Text.pack (show e))))

fallbackTo :: Eff es (Either e a) -> Eff es (Either e a) -> Eff es (Either e a)
fallbackTo action fallback = action >>= either (const fallback) (pure . Right)
