module WorkerSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Exception (throwIO)
import Effectful.State.Static.Local
import Test.Tasty
import Test.Tasty.HUnit

import Fixtures (anError, installJob, installMutation)
import TestInterpreters (GhcupHandlers (..), idleHandlers, runGhcupTest, runNotifyCollect)
import Toolchain.Types
import Worker (processJob)

type TestEs = '[State Int]

emptyReady :: Either OpError (Listings, Bool)
emptyReady = Right (Map.empty, False)

-- | Run jobs through 'processJob' with a fake ghcup. Returns the emitted
-- messages and the final value of the test counter (bumped by whichever
-- handler a test wires 'bump' into).
run :: GhcupHandlers TestEs -> [Job] -> ([UiMsg], Int)
run handlers jobs =
  let ((_, msgs), count) =
        runPureEff
          . runState (0 :: Int)
          . runGhcupTest handlers
          . runNotifyCollect
          $ mapM_ (processJob (\_ -> pure ())) jobs
  in (msgs, count)

bump :: (State Int :> es) => Eff es ()
bump = modify (\n -> n + (1 :: Int))

jobDones :: [UiMsg] -> [UiMsg]
jobDones = filter $ \case
  JobDone _ _ -> True
  _ -> False

tests :: TestTree
tests =
  testGroup
    "Worker"
    [ testGroup
        "env acquisition"
        [ testCase "failure on a mutation: exactly one JobDone, relist never attempted" $ do
            let handlers =
                  idleHandlers
                    { acquire = pure (Left anError)
                    , relist = bump >> pure emptyReady
                    }
                (msgs, count) = run handlers [installJob]
            msgs @?= [JobDone installMutation (Left anError)]
            count @?= 0
        , testCase "failure on a refresh: ListingsFailed only" $ do
            let (msgs, _) = run idleHandlers {acquire = pure (Left anError)} [RefreshListings]
            msgs @?= [ListingsFailed anError]
        , testCase "a failed env build is retried on the next job" $ do
            let handlers = idleHandlers {acquire = bump >> pure (Left anError)}
                (_, count) = run handlers [installJob, installJob]
            count @?= 2
        , testCase "a held env is never rebuilt" $ do
            let handlers = idleHandlers {acquire = bump >> pure (Right ())}
                (_, count) = run handlers [RefreshListings, RefreshListings]
            count @?= 1
        ]
    , testGroup
        "refresh"
        [ testCase "happy path emits ListingsReady" $ do
            let (msgs, _) = run idleHandlers [RefreshListings]
            msgs @?= [ListingsReady Map.empty False]
        , testCase "listing failure emits ListingsFailed" $ do
            let (msgs, _) = run idleHandlers {getListings = pure (Left anError)} [RefreshListings]
            msgs @?= [ListingsFailed anError]
        ]
    , testGroup
        "mutations"
        [ testCase "happy path: one JobDone, then the relist result" $ do
            let (msgs, _) = run idleHandlers [installJob]
            msgs
              @?= [ JobDone installMutation (Right ())
                  , ListingsReady Map.empty False
                  ]
        , testCase "failure: one JobDone, relist never attempted" $ do
            let handlers =
                  idleHandlers
                    { install = \_ _ -> pure (Left anError)
                    , relist = bump >> pure emptyReady
                    }
                (msgs, count) = run handlers [installJob]
            msgs @?= [JobDone installMutation (Left anError)]
            count @?= 0
        , testCase "relist failure degrades to a full refresh" $ do
            let (msgs, _) = run idleHandlers {relist = pure (Left anError)} [installJob]
            msgs
              @?= [ JobDone installMutation (Right ())
                  , ListingsReady Map.empty False
                  ]
        , testCase "relist and refresh both failing ends in ListingsFailed, never a second JobDone" $ do
            let handlers =
                  idleHandlers
                    { relist = pure (Left anError)
                    , getListings = pure (Left anError)
                    }
                (msgs, _) = run handlers [installJob]
            msgs
              @?= [ JobDone installMutation (Right ())
                  , ListingsFailed anError
                  ]
        , testCase "an operation that throws maps to one Unexpected-error JobDone" $ do
            let handlers = idleHandlers {install = \_ _ -> throwIO (userError "disk on fire")}
                (msgs, _) = run handlers [installJob]
            length (jobDones msgs) @?= 1
            case msgs of
              [JobDone job (Left err)] -> do
                job @?= installMutation
                err.title @?= ("Unexpected error" :: Text)
              other -> assertFailure ("unexpected emissions: " <> show other)
        , testCase "env survives an operation exception" $ do
            let handlers =
                  idleHandlers
                    { acquire = bump >> pure (Right ())
                    , install = \_ _ -> throwIO (userError "disk on fire")
                    }
                (_, count) = run handlers [installJob, RefreshListings]
            count @?= 1
        ]
    , testGroup
        "progress lines"
        [ testCase "a percentage becomes a fraction" $
            progressOf "downloading 42%" @?= Progress "downloading 42%" (Just 0.42)
        , testCase "decimal percentages parse" $
            (progressOf "12.5%").fraction @?= Just 0.125
        , testCase "the phase text is stripped" $
            (progressOf "  unpacking \n").phase @?= "unpacking"
        , testCase "no percentage → no fraction" $
            (progressOf "unpacking").fraction @?= Nothing
        , testCase "out-of-range percentages are ignored" $
            (progressOf "999%").fraction @?= Nothing
        ]
    ]
