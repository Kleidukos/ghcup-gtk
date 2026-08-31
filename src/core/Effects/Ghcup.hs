module Effects.Ghcup
  ( Ghcup (..)
  , fetchListings
  , relistListings
  , installTool
  , uninstallTool
  , setDefaultVersion
  , compileGhcTool
  , compileHlsTool
  , reconfigureSources
  , runGhcupIO
  ) where

import Data.IORef
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import GHCup.Types (NewURLSource, TargetVersion, TargetVersionReq, Tool)

import Toolchain.GHCup (GhcupEnv)
import Toolchain.GHCup qualified as GHCup
import Toolchain.Types (CompileGhcOptions, CompileHlsOptions, Freshness, InstallOptions, Listings, OpError (..))

-- | The ghcup domain operations.
data Ghcup :: Effect where
  FetchListings :: Ghcup m (Either OpError (Listings, Freshness))
  RelistListings :: Ghcup m (Either OpError Listings)
  InstallTool :: Tool -> TargetVersionReq -> InstallOptions -> Ghcup m (Either OpError ())
  UninstallTool :: Tool -> TargetVersion -> Ghcup m (Either OpError ())
  SetDefaultVersion :: Tool -> TargetVersion -> Ghcup m (Either OpError ())
  CompileGhcTool :: TargetVersion -> CompileGhcOptions -> Ghcup m (Either OpError ())
  CompileHlsTool :: TargetVersion -> CompileHlsOptions -> Ghcup m (Either OpError ())
  ReconfigureSources :: [NewURLSource] -> Ghcup m ()

type instance DispatchOf Ghcup = Dynamic

fetchListings :: (Ghcup :> es) => Eff es (Either OpError (Listings, Freshness))
fetchListings = send FetchListings

relistListings :: (Ghcup :> es) => Eff es (Either OpError Listings)
relistListings = send RelistListings

installTool :: (Ghcup :> es) => Tool -> TargetVersionReq -> InstallOptions -> Eff es (Either OpError ())
installTool tool tvr opts = send (InstallTool tool tvr opts)

uninstallTool :: (Ghcup :> es) => Tool -> TargetVersion -> Eff es (Either OpError ())
uninstallTool tool tv = send (UninstallTool tool tv)

setDefaultVersion :: (Ghcup :> es) => Tool -> TargetVersion -> Eff es (Either OpError ())
setDefaultVersion tool tv = send (SetDefaultVersion tool tv)

compileGhcTool :: (Ghcup :> es) => TargetVersion -> CompileGhcOptions -> Eff es (Either OpError ())
compileGhcTool tv opts = send (CompileGhcTool tv opts)

compileHlsTool :: (Ghcup :> es) => TargetVersion -> CompileHlsOptions -> Eff es (Either OpError ())
compileHlsTool tv opts = send (CompileHlsTool tv opts)

reconfigureSources :: (Ghcup :> es) => [NewURLSource] -> Eff es ()
reconfigureSources = send . ReconfigureSources

runGhcupIO :: (IOE :> es) => [NewURLSource] -> (Text -> IO ()) -> Eff (Ghcup : es) a -> Eff es a
runGhcupIO urlSource onLog action = do
  sourcesRef <- liftIO (newIORef urlSource)
  envRef <- liftIO (newIORef Nothing)
  let withEnv :: forall b. (GhcupEnv -> IO (Either OpError b)) -> IO (Either OpError b)
      withEnv op =
        readIORef envRef >>= \case
          Just env -> op env
          Nothing -> do
            sources <- readIORef sourcesRef
            GHCup.newEnv sources onLog >>= \case
              Left err -> pure (Left err)
              Right env -> writeIORef envRef (Just env) >> op env
  interpret
    ( \_ -> \case
        FetchListings -> liftIO (withEnv GHCup.getListings)
        RelistListings -> liftIO (withEnv GHCup.relistListings)
        InstallTool tool tvr opts -> liftIO (withEnv (\env -> GHCup.install env tool tvr opts))
        UninstallTool tool tv -> liftIO (withEnv (\env -> GHCup.uninstall env tool tv))
        SetDefaultVersion tool tv -> liftIO (withEnv (\env -> GHCup.setDefault env tool tv))
        CompileGhcTool tv opts -> liftIO (withEnv (\env -> GHCup.compileGhc env tv opts))
        CompileHlsTool tv opts -> liftIO (withEnv (\env -> GHCup.compileHls env tv opts))
        ReconfigureSources sources -> liftIO $ do
          writeIORef sourcesRef sources
          readIORef envRef >>= mapM_ (\env -> GHCup.setUrlSource env sources)
    )
    action
