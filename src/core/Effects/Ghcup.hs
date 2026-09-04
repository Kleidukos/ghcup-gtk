module Effects.Ghcup
  ( Ghcup (..)
  , fetchListings
  , relistListings
  , installTool
  , uninstallTool
  , setDefaultVersion
  , runGhcupIO
  ) where

import Data.IORef
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import GHCup.Types (TargetVersion, TargetVersionReq, Tool)

import Toolchain.GHCup (GhcupEnv)
import Toolchain.GHCup qualified as GHCup
import Toolchain.Types (InstallOptions, Listings, OpError (..))

-- | The ghcup domain operations.
data Ghcup :: Effect where
  FetchListings :: Ghcup m (Either OpError (Listings, Bool))
  RelistListings :: Ghcup m (Either OpError (Listings, Bool))
  InstallTool :: Tool -> TargetVersionReq -> InstallOptions -> Ghcup m (Either OpError ())
  UninstallTool :: Tool -> TargetVersion -> Ghcup m (Either OpError ())
  SetDefaultVersion :: Tool -> TargetVersion -> Ghcup m (Either OpError ())

type instance DispatchOf Ghcup = Dynamic

fetchListings :: (Ghcup :> es) => Eff es (Either OpError (Listings, Bool))
fetchListings = send FetchListings

relistListings :: (Ghcup :> es) => Eff es (Either OpError (Listings, Bool))
relistListings = send RelistListings

installTool :: (Ghcup :> es) => Tool -> TargetVersionReq -> InstallOptions -> Eff es (Either OpError ())
installTool tool tvr opts = send (InstallTool tool tvr opts)

uninstallTool :: (Ghcup :> es) => Tool -> TargetVersion -> Eff es (Either OpError ())
uninstallTool tool tv = send (UninstallTool tool tv)

setDefaultVersion :: (Ghcup :> es) => Tool -> TargetVersion -> Eff es (Either OpError ())
setDefaultVersion tool tv = send (SetDefaultVersion tool tv)

runGhcupIO :: (IOE :> es) => (Text -> Eff es ()) -> Eff (Ghcup : es) a -> Eff es a
runGhcupIO onLog action =
  withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift -> do
    envRef <- newIORef Nothing
    let withEnv :: forall b. (GhcupEnv -> IO (Either OpError b)) -> IO (Either OpError b)
        withEnv op =
          readIORef envRef >>= \case
            Just env -> op env
            Nothing ->
              GHCup.newEnv (unlift . onLog) >>= \case
                Left err -> pure (Left err)
                Right env -> writeIORef envRef (Just env) >> op env
    unlift $
      interpret
        ( \_ -> \case
            FetchListings -> liftIO (withEnv GHCup.getListings)
            RelistListings -> liftIO (withEnv GHCup.relistListings)
            InstallTool tool tvr opts -> liftIO (withEnv (\env -> GHCup.install env tool tvr opts))
            UninstallTool tool tv -> liftIO (withEnv (\env -> GHCup.uninstall env tool tv))
            SetDefaultVersion tool tv -> liftIO (withEnv (\env -> GHCup.setDefault env tool tv))
        )
        action
