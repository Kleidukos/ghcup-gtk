module Toolchain.GHCup
  ( GhcupEnv
  , newEnv
  , getListings
  , relistListings
  , install
  , uninstall
  , setDefault
  , ghcupDirs
  ) where

import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Functor ((<&>))
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Variant.Excepts
import Data.Vector qualified as Vector
import GHCup.Command.Install (installTool)
import GHCup.Command.List
import GHCup.Command.Rm (rmToolVersion)
import GHCup.Command.Set (setToolVersion)
import GHCup.Download (getDownloadsF)
import GHCup.Errors
import GHCup.Query.GHCupDirs (fromGHCupPath, getAllDirs)
import GHCup.Query.System (platformRequest)
import GHCup.Setup (ensureDirectories)
import GHCup.Types
import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)

import Toolchain.Types

data GhcupEnv = GhcupEnv
  { appStateRef :: IORef AppState
  , staleRef :: IORef Bool
  }

renderErr :: Text -> String -> OpError
renderErr title = OpError title . Text.pack

-- | Collapse a ghcup variant error into an 'OpError' under one title.
toOpError :: (Pretty (V es)) => Text -> VEither es a -> Either OpError a
toOpError title = \case
  VRight a -> Right a
  VLeft e -> Left (renderErr title (prettyShow e))

runIn :: GhcupEnv -> (AppState -> IO r) -> IO r
runIn env act = readIORef env.appStateRef >>= act

ghcupDirs :: IO GhcupDirs
ghcupDirs = do
  dirs <- getAllDirs
  pure
    GhcupDirs
      { ghcupBinDir = binDir dirs
      , ghcupBaseDir = fromGHCupPath (baseDir dirs)
      }

newEnv :: (Text -> IO ()) -> IO (Either OpError GhcupEnv)
newEnv logSink = do
  dirs <- getAllDirs
  ensureDirectories dirs

  let settings = defaultSettings {cache = True, metaMode = Strict}
      loggerConfig =
        LoggerConfig
          { lcPrintDebugLvl = Nothing
          , consoleOutter = logSink
          , fileOutter = \_ -> pure ()
          , fancyColors = False
          }
  pfreqE <-
    flip runReaderT loggerConfig
      . runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound]
      $ liftE platformRequest
  case toOpError "Unsupported platform" pfreqE of
    Left err -> pure (Left err)
    Right pfreq -> do
      let emptyInfo = GHCupInfo mempty (GHCupDownloads mempty) Nothing
      appStateRef <- newIORef (AppState settings dirs defaultKeyBindings emptyInfo pfreq loggerConfig)
      staleRef <- newIORef False
      pure (Right GhcupEnv {appStateRef, staleRef})

fetchInfo :: LeanAppState -> PlatformRequest -> IO (Either OpError (GHCupInfo, Bool))
fetchInfo lean pfreq = do
  first <- runFetch lean
  case first of
    VRight gi -> pure (Right (gi, False))
    VLeft _ -> do
      let LeanAppState {settings = onlineSettings, dirs, keyBindings, pfreq = p, loggerConfig} = lean
          offline =
            LeanAppState (onlineSettings {noNetwork = True}) dirs keyBindings p loggerConfig
      second <- runFetch offline
      pure (fmap (,True) (toOpError "Could not fetch toolchain metadata" second))
  where
    runFetch env =
      flip runReaderT env
        . runE
          @'[ DigestError
            , ContentLengthError
            , GPGError
            , JSONError
            , DownloadFailed
            , FileDoesNotExistError
            , StackPlatformDetectError
            , UnsupportedMetadataFormat
            ]
        $ liftE (getDownloadsF pfreq)

getListings :: GhcupEnv -> IO (Either OpError (Listings, Bool))
getListings env = do
  appState <- readIORef env.appStateRef
  let AppState {settings, dirs, keyBindings, pfreq, loggerConfig} = appState
      lean = LeanAppState settings dirs keyBindings pfreq loggerConfig
  fetchInfo lean pfreq >>= \case
    Left err -> pure (Left err)
    Right (ghcupInfo, stale) -> do
      let refreshed = appState {ghcupInfo = ghcupInfo} :: AppState
      writeIORef env.appStateRef refreshed
      writeIORef env.staleRef stale
      fmap (,stale) <$> runList refreshed

relistListings :: GhcupEnv -> IO (Either OpError (Listings, Bool))
relistListings env = do
  appState <- readIORef env.appStateRef
  stale <- readIORef env.staleRef
  fmap (,stale) <$> runList appState

runList :: AppState -> IO (Either OpError Listings)
runList appState = do
  result <-
    flip runReaderT appState
      . runE @'[ParseError]
      $ liftE
      $ listVersions
        Nothing
        []
        ShowNone
        False
        NShowNone
        (Nothing, Nothing)
  pure $
    toOpError "Could not list versions" result
      <&> Map.map (Vector.fromList . snd)

install :: GhcupEnv -> Tool -> TargetVersionReq -> IO (Either OpError ())
install env tool tvr = runIn env $ \appState -> do
  result <-
    flip runReaderT appState
      . runResourceT
      . runE
        @'[ AlreadyInstalled
          , CopyError
          , DigestError
          , ContentLengthError
          , GPGError
          , DownloadFailed
          , NoDownload
          , NotInstalled
          , UnknownArchive
          , TarDirDoesNotExist
          , ArchiveResult
          , FileAlreadyExistsError
          , URIParseError
          , NoInstallInfo
          , MergeFileTreeError
          , ProcessError
          , ParseError
          , DirNotEmpty
          , UninstallFailed
          , MalformedInstallInfo
          , InvalidBuildConfig
          ]
      $ liftE (installTool tool tvr GHCupInternal False [] Nothing)
  pure $ case result of
    VLeft (V (AlreadyInstalled _ _)) -> Right ()
    other -> void (toOpError "Installation failed" other)

uninstall :: GhcupEnv -> Tool -> TargetVersion -> IO (Either OpError ())
uninstall env tool tv = runIn env $ \appState ->
  toOpError "Uninstall failed"
    <$> ( flip runReaderT appState
            . runE @'[NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo]
            $ liftE (rmToolVersion tool tv)
        )

setDefault :: GhcupEnv -> Tool -> TargetVersion -> IO (Either OpError ())
setDefault env tool tv = runIn env $ \appState -> do
  result <-
    flip runReaderT appState
      . runE @'[ParseError, NotInstalled]
      $ liftE (setToolVersion tool tv)
  pure (void (toOpError "Could not set default" result))
