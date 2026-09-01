module Effects.HostEnv
  ( HostEnv (..)
  , getHostEnvironment
  , runHostEnvIO
  ) where

import Control.Monad (mfilter)
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception (IOException, try)
import Effectful.Process.Typed (TypedProcess, proc, readProcess)
import Effectful.Timeout (Timeout, timeout)
import System.Environment qualified as System
import System.Exit (ExitCode (..))

data HostEnv :: Effect where
  GetHostEnvironment :: HostEnv m (Maybe Text, Maybe Text)

type instance DispatchOf HostEnv = Dynamic

getHostEnvironment :: (HostEnv :> es) => Eff es (Maybe Text, Maybe Text)
getHostEnvironment = send GetHostEnvironment

runHostEnvIO :: (TypedProcess :> es, Timeout :> es, IOE :> es) => Eff (HostEnv : es) a -> Eff es a
runHostEnvIO = interpret $ \_ -> \case
  GetHostEnvironment ->
    liftIO (System.lookupEnv "FLATPAK_ID") >>= \case
      Nothing -> pure (Nothing, Nothing)
      Just _ ->
        hostQuery "sh" [] "$SHELL" >>= \case
          Nothing -> pure (Nothing, Nothing)
          Just shell -> do
            path <- hostQuery (Text.unpack shell) ["-i", "-l"] "$PATH"
            pure (Just shell, path)

hostQuery :: (TypedProcess :> es, Timeout :> es) => FilePath -> [String] -> String -> Eff es (Maybe Text)
hostQuery shell flags var = do
  let script = "printf '%s' \"" <> var <> "\""
  result <-
    timeout 3_000_000 . try @IOException . readProcess $
      proc "flatpak-spawn" (["--host", shell] <> flags <> ["-c", script])
  pure $ case result of
    Just (Right (ExitSuccess, out, _)) ->
      out
        & LBS.toStrict
        & Text.Encoding.decodeUtf8Lenient
        & Text.takeWhileEnd (/= '\n')
        & Text.strip
        & Just
        & mfilter (not . Text.null)
    _ -> Nothing
