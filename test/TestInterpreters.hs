-- | Pure interpreters for the application's effects.
module TestInterpreters
  ( runNotifyCollect
  , GhcupHandlers (..)
  , idleHandlers
  , runGhcupTest
  , runFileSystemPure
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import GHCup.Types (TargetVersion, TargetVersionReq, Tool)
import System.FilePath ((</>))

import Effects.FileSystem (FileSystem (..))
import Effects.Ghcup (Ghcup (..))
import Effects.Notify (Notify (..))
import Toolchain.Types (CompileGhcOptions, CompileHlsOptions, Freshness (..), InstallOptions, Listings, OpError, UiMsg)

-- | Record every emitted 'UiMsg' in order.
runNotifyCollect :: Eff (Notify : es) a -> Eff es (a, [UiMsg])
runNotifyCollect = reinterpret (runState []) $ \_ -> \case
  Emit msg -> modify (<> [msg])

-- | Responses of a fake ghcup: the environment acquisition every
-- operation runs behind, then one field per operation. Fields live in
-- 'Eff es' so a test can thread its own counters through them.
data GhcupHandlers es = GhcupHandlers
  { acquire :: Eff es (Either OpError ())
  , getListings :: Eff es (Either OpError (Listings, Freshness))
  , relist :: Eff es (Either OpError Listings)
  , install :: Tool -> TargetVersionReq -> InstallOptions -> Eff es (Either OpError ())
  , uninstall :: Tool -> TargetVersion -> Eff es (Either OpError ())
  , setDefault :: Tool -> TargetVersion -> Eff es (Either OpError ())
  , compileGhc :: TargetVersion -> CompileGhcOptions -> Eff es (Either OpError ())
  , compileHls :: TargetVersion -> CompileHlsOptions -> Eff es (Either OpError ())
  }

idleHandlers :: GhcupHandlers es
idleHandlers =
  GhcupHandlers
    { acquire = pure (Right ())
    , getListings = pure (Right (Map.empty, Fresh))
    , relist = pure (Right Map.empty)
    , install = \_ _ _ -> pure (Right ())
    , uninstall = \_ _ -> pure (Right ())
    , setDefault = \_ _ -> pure (Right ())
    , compileGhc = \_ _ -> pure (Right ())
    , compileHls = \_ _ -> pure (Right ())
    }

-- | Mirrors the live interpreter's memoization: a successful acquisition
-- is cached, a failed one is retried by the next operation.
runGhcupTest :: forall es a. GhcupHandlers es -> Eff (Ghcup : es) a -> Eff es a
runGhcupTest h = reinterpret (evalState False) $ \_ -> \case
  FetchListings -> acquiring h.getListings
  RelistListings -> acquiring h.relist
  InstallTool tool tvr opts -> acquiring (h.install tool tvr opts)
  UninstallTool tool tv -> acquiring (h.uninstall tool tv)
  SetDefaultVersion tool tv -> acquiring (h.setDefault tool tv)
  CompileGhcTool tv opts -> acquiring (h.compileGhc tv opts)
  CompileHlsTool tv opts -> acquiring (h.compileHls tv opts)
  where
    acquiring :: Eff es (Either OpError b) -> Eff (State Bool : es) (Either OpError b)
    acquiring op =
      get >>= \case
        True -> raise op
        False ->
          raise h.acquire >>= \case
            Left err -> pure (Left err)
            Right () -> put True >> raise op

runFileSystemPure
  :: Map String String
  -- ^ Environment variables.
  -> Map FilePath Text
  -> [FilePath]
  -- ^ Unreadable files: they exist, but reading them fails.
  -> Eff (FileSystem : es) a
  -> Eff es (a, Map FilePath Text)
runFileSystemPure vars initial unreadable = reinterpret (runState initial) $ \_ -> \case
  DoesFileExist path ->
    gets @(Map FilePath Text) (\files -> Map.member path files || path `elem` unreadable)
  ReadFileText path
    | path `elem` unreadable -> pure (Left ("unreadable: " <> Text.pack path))
    | otherwise ->
        gets (maybe (Left ("no such file: " <> Text.pack path)) Right . Map.lookup path)
  WriteFileAtomic path payload -> Right () <$ modify (Map.insert path payload)
  GetXdgDirectory _ sub -> pure ("/fake/xdg" </> sub)
  GetHomeDirectory -> pure "/fake/home"
  LookupEnv name -> pure (Map.lookup name vars)
