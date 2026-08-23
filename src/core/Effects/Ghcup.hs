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
import GHCup.Types (TargetVersion, TargetVersionReq)

import Toolchain.GHCup (GhcupEnv)
import Toolchain.GHCup qualified as GHCup
import Toolchain.Types (Listings, OpError (..), SupportedTool)

-- | The ghcup domain operations.
data Ghcup :: Effect where
  FetchListings :: Ghcup m (Either OpError (Listings, Bool))
  RelistListings :: Ghcup m (Either OpError (Listings, Bool))
  InstallTool :: SupportedTool -> TargetVersionReq -> Ghcup m (Either OpError ())
  UninstallTool :: SupportedTool -> TargetVersion -> Ghcup m (Either OpError ())
  SetDefaultVersion :: SupportedTool -> TargetVersion -> Ghcup m (Either OpError ())

type instance DispatchOf Ghcup = Dynamic

fetchListings :: (Ghcup :> es) => Eff es (Either OpError (Listings, Bool))
fetchListings = send FetchListings

relistListings :: (Ghcup :> es) => Eff es (Either OpError (Listings, Bool))
relistListings = send RelistListings

installTool :: (Ghcup :> es) => SupportedTool -> TargetVersionReq -> Eff es (Either OpError ())
installTool tool tvr = send (InstallTool tool tvr)

uninstallTool :: (Ghcup :> es) => SupportedTool -> TargetVersion -> Eff es (Either OpError ())
uninstallTool tool tv = send (UninstallTool tool tv)

setDefaultVersion :: (Ghcup :> es) => SupportedTool -> TargetVersion -> Eff es (Either OpError ())
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
            InstallTool tool tvr -> liftIO (withEnv (\env -> GHCup.install env tool tvr))
            UninstallTool tool tv -> liftIO (withEnv (\env -> GHCup.uninstall env tool tv))
            SetDefaultVersion tool tv -> liftIO (withEnv (\env -> GHCup.setDefault env tool tv))
        )
        action
