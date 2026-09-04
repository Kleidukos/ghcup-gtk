module Toolchain.GHCup
  ( GhcupEnv
  , loadUrlSource
  , saveUrlSource
  , setUrlSource
  , updateUrlSource
  , newEnv
  , getListings
  , relistListings
  , install
  , uninstall
  , setDefault
  , compileGhc
  , compileHls
  , ghcupDirs
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Variant.Excepts
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml
import GHCup.Command.Compile.GHC qualified as CompileGHC
import GHCup.Command.Compile.HLS qualified as CompileHLS
import GHCup.Command.Install (installBindist, installTool)
import GHCup.Command.List
import GHCup.Command.Rm (rmToolVersion)
import GHCup.Command.Set (setToolVersion)
import GHCup.Download (getDownloadsF)
import GHCup.Errors
import GHCup.Query.GHCupDirs (fromGHCupPath, getAllDirs, getConfigFilePath, ghcupConfigFile)
import GHCup.Query.Metadata (getDownloadInfoE')
import GHCup.Query.System (platformRequest)
import GHCup.Setup (ensureDirectories)
import GHCup.Types
import GHCup.Types.JSON ()
import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)
import URI.ByteString (URI)

import Effects.FileSystem (atomicWriteBytes)
import Toolchain.Channels (BaseChannel, Channel, applyUrlSource, uriText)
import Toolchain.Types

-- | Access 'appStateRef' only on the worker thread. 'setUrlSource' and
-- 'getListings' both read-modify-write it.
newtype GhcupEnv = GhcupEnv
  { appStateRef :: IORef AppState
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

-- | The url-source of the ghcup config of the user. If the config is not
-- readable, the default channel and a warning.
loadUrlSource :: IO ([NewURLSource], Maybe Text)
loadUrlSource =
  try @SomeException (runE @'[JSONError] ghcupConfigFile) <&> \case
    Right (VRight userSettings) -> (urlSourceOf userSettings, Nothing)
    Right (VLeft err) -> degraded (Text.pack (prettyShow err))
    Left err -> degraded (Text.pack (show err))
  where
    degraded reason =
      ( defaultSettings.urlSource
      , Just ("Could not read the ghcup config, using the default channel: " <> reason)
      )

-- | The url-source list of a ghcup config, with the same default as ghcup.
urlSourceOf :: UserSettings -> [NewURLSource]
urlSourceOf userSettings =
  maybe defaultSettings.urlSource fromURLSource userSettings.uUrlSource

updateUrlSource :: Maybe BaseChannel -> Set Channel -> Maybe URI -> UserSettings -> UserSettings
updateUrlSource base requested nightlies userSettings =
  userSettings
    { uUrlSource =
        Just (SimpleList (applyUrlSource base requested nightlies (urlSourceOf userSettings)))
    }

-- | Enable the requested channels, and the base if given, in the ghcup config.
saveUrlSource :: Maybe BaseChannel -> Set Channel -> Maybe URI -> IO (Either Text [NewURLSource])
saveUrlSource base requested nightlies =
  try @SomeException write <&> either (Left . Text.pack . show) id
  where
    write =
      runE @'[JSONError] ghcupConfigFile >>= \case
        VLeft err ->
          pure (Left ("Not overwriting an unreadable ghcup config: " <> Text.pack (prettyShow err)))
        VRight userSettings -> do
          let updated = updateUrlSource base requested nightlies userSettings
          path <- getConfigFilePath
          atomicWriteBytes path (Yaml.encode updated)
          pure (Right (urlSourceOf updated))

-- | Replace the in-memory url-source of the worker. Does not write to disk.
setUrlSource :: GhcupEnv -> [NewURLSource] -> IO ()
setUrlSource env urlSource =
  modifyIORef' env.appStateRef $ \appState ->
    appState {settings = appState.settings {urlSource}} :: AppState

newEnv :: [NewURLSource] -> (Text -> IO ()) -> IO (Either OpError GhcupEnv)
newEnv urlSource logSink = do
  dirs <- getAllDirs
  ensureDirectories dirs

  let settings = defaultSettings {cache = True, metaMode = Strict, urlSource}
      loggerConfig =
        LoggerConfig
          { lcPrintDebugLvl = Nothing
          , consoleOutter = logSink
          , fileOutter = \_ -> pure ()
          , fancyColors = False
          }
  pfreqE <-
    liftE platformRequest
      & runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound]
      & flip runReaderT loggerConfig
  case toOpError "Unsupported platform" pfreqE of
    Left err -> pure (Left err)
    Right pfreq -> do
      let emptyInfo = GHCupInfo mempty (GHCupDownloads mempty) Nothing
      appStateRef <- newIORef (AppState settings dirs defaultKeyBindings emptyInfo pfreq loggerConfig)
      pure (Right GhcupEnv {appStateRef})

fetchInfo :: LeanAppState -> PlatformRequest -> IO (Either OpError (GHCupInfo, Freshness))
fetchInfo lean pfreq = do
  first <- runFetch lean
  case first of
    VRight gi -> pure (Right (gi, Fresh))
    VLeft _ -> do
      let LeanAppState {settings = onlineSettings, dirs, keyBindings, pfreq = p, loggerConfig} = lean
          offline =
            LeanAppState (onlineSettings {noNetwork = True}) dirs keyBindings p loggerConfig
      second <- runFetch offline
      pure (fmap (,Stale) (toOpError "Could not fetch toolchain metadata" second))
  where
    runFetch env =
      liftE (getDownloadsF pfreq)
        & runE
          @'[ DigestError
            , ContentLengthError
            , GPGError
            , JSONError
            , DownloadFailed
            , FileDoesNotExistError
            , StackPlatformDetectError
            , UnsupportedMetadataFormat
            ]
        & flip runReaderT env

getListings :: GhcupEnv -> IO (Either OpError (Listings, Freshness))
getListings env = do
  appState <- readIORef env.appStateRef
  let AppState {settings, dirs, keyBindings, pfreq, loggerConfig} = appState
      lean = LeanAppState settings dirs keyBindings pfreq loggerConfig
  fetchInfo lean pfreq >>= \case
    Left err -> pure (Left err)
    Right (ghcupInfo, freshness) -> do
      let refreshed = appState {ghcupInfo = ghcupInfo} :: AppState
      writeIORef env.appStateRef refreshed
      fmap (,freshness) <$> runList refreshed

relistListings :: GhcupEnv -> IO (Either OpError Listings)
relistListings env = readIORef env.appStateRef >>= runList

runList :: AppState -> IO (Either OpError Listings)
runList appState = do
  result <-
    listVersions
      Nothing
      []
      ShowNone
      False
      NShowAll
      (Nothing, Nothing)
      & liftE
      & runE @'[ParseError]
      & flip runReaderT appState
  pure $
    toOpError "Could not list versions" result
      <&> Map.map (Vector.fromList . snd)

bindistTarDir :: Tool -> Maybe TarDir
bindistTarDir = \case
  Tool "ghc" -> Just (RegexDir "ghc-.*")
  Tool "hls" -> Just (RegexDir "haskell-language-server-*")
  _ -> Nothing

install :: GhcupEnv -> Tool -> TargetVersionReq -> InstallOptions -> IO (Either OpError ())
install env tool tvr opts = runIn env $ \appState -> do
  let effectiveState = case opts.bindistUrl of
        Nothing -> appState
        Just _ -> appState {settings = appState.settings {noVerify = True}} :: AppState
      installAction = case opts.bindistUrl of
        Nothing ->
          liftE (void $ installTool tool tvr opts.installDir opts.forceInstall opts.extraConfArgs opts.installTargets)
        Just uri -> do
          rev <- case tvr of
            TargetVersionReq _ (Just r) -> pure r
            TargetVersionReq _ Nothing -> do
              revE <- lift (runE @'[NoDownload] (getDownloadInfoE' tool tvr))
              pure $ case revE of
                VRight (r, _) -> r
                VLeft _ -> 0
          let dlInfo = DownloadInfo (uriText uri) (bindistTarDir tool) "" Nothing Nothing Nothing Nothing
              GHCupInfo {_ghcupDownloads = dls} = appState.ghcupInfo
              toolDesc = Map.lookup tool (unGHCupDownloads dls) >>= _toolDetails
              TargetVersionReq tv _ = tvr
          liftE
            ( void $
                installBindist
                  tool
                  toolDesc
                  dlInfo
                  (TargetVersionRev tv rev)
                  opts.installDir
                  opts.forceInstall
                  opts.extraConfArgs
                  opts.installTargets
            )
  result <-
    installAction
      & runE
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
      & runResourceT
      & flip runReaderT effectiveState
  let installOutcome = case result of
        VLeft (V (AlreadyInstalled _ _)) -> Right ()
        other -> void (toOpError "Installation failed" other)
  case installOutcome of
    Left err -> pure (Left err)
    Right ()
      | opts.setAsDefault && opts.installDir == GHCupInternal -> do
          setResult <-
            liftE (setToolVersion tool tvr._tvqTargetVer)
              & runE @'[ParseError, NotInstalled]
              & flip runReaderT appState
          pure (void (toOpError "Installed, but could not set as default" setResult))
      | otherwise -> pure (Right ())

compileGhc :: GhcupEnv -> TargetVersion -> CompileGhcOptions -> IO (Either OpError ())
compileGhc env tv opts = runIn env $ \appState -> do
  let ghcVer = case opts.gitRef of
        Just ref -> CompileGHC.GitDist (GitBranch ref Nothing)
        Nothing -> CompileGHC.SourceDist tv._tvVersion
  result <-
    CompileGHC.compileGHC
      ghcVer
      opts.crossTarget
      opts.overwriteVer
      opts.bootstrapGhc
      opts.hadrianGhc
      opts.jobs
      opts.buildConfig
      opts.patches
      opts.addConfArgs
      opts.buildFlavour
      opts.buildSystem
      (maybe GHCupInternal IsolateDir opts.isolateDir)
      opts.installTargets
      opts.docs
      & liftE
      & runE
        @'[ AlreadyInstalled
          , BuildFailed
          , DigestError
          , ContentLengthError
          , GPGError
          , DownloadFailed
          , GHCupSetError
          , NoDownload
          , NotFoundInPATH
          , PatchFailed
          , UnknownArchive
          , TarDirDoesNotExist
          , NotInstalled
          , DirNotEmpty
          , ArchiveResult
          , FileDoesNotExistError
          , HadrianNotFound
          , InvalidBuildConfig
          , ProcessError
          , CopyError
          , UninstallFailed
          , MergeFileTreeError
          , URIParseError
          , ParseError
          , FileAlreadyExistsError
          , NoInstallInfo
          , MalformedInstallInfo
          ]
      & runResourceT
      & flip runReaderT appState
  setAfterCompile
    appState
    (opts.setCompile && isNothing opts.isolateDir)
    ghc
    (toOpError "GHC compilation failed" result)

compileHls :: GhcupEnv -> TargetVersion -> CompileHlsOptions -> IO (Either OpError ())
compileHls env tv opts = runIn env $ \appState -> do
  let hlsVer = case opts.gitRef of
        Just ref -> CompileHLS.GitDist (GitBranch ref Nothing)
        Nothing -> CompileHLS.SourceDist tv._tvVersion
  result <-
    CompileHLS.compileHLS
      hlsVer
      opts.targetGhcs
      opts.jobs
      opts.overwriteVer
      (maybe GHCupInternal IsolateDir opts.isolateDir)
      opts.cabalProject
      opts.cabalProjectLocal
      opts.updateCabal
      opts.patches
      opts.cabalArgs
      & liftE
      & runE
        @'[ NoDownload
          , GPGError
          , DownloadFailed
          , DigestError
          , ContentLengthError
          , UnknownArchive
          , TarDirDoesNotExist
          , ArchiveResult
          , BuildFailed
          , NotInstalled
          , URIParseError
          ]
      & runResourceT
      & flip runReaderT appState
  setAfterCompile
    appState
    (opts.setCompile && isNothing opts.isolateDir)
    hls
    (fmap mkTVer (toOpError "HLS compilation failed" result))

setAfterCompile :: AppState -> Bool -> Tool -> Either OpError TargetVersion -> IO (Either OpError ())
setAfterCompile appState setCompile tool = \case
  Left err -> pure (Left err)
  Right targetVer
    | setCompile -> do
        setResult <-
          liftE (setToolVersion tool targetVer)
            & runE @'[ParseError, NotInstalled]
            & flip runReaderT appState
        pure (void (toOpError "Compiled, but could not set as default" setResult))
    | otherwise -> pure (Right ())

uninstall :: GhcupEnv -> Tool -> TargetVersion -> IO (Either OpError ())
uninstall env tool tv = runIn env $ \appState ->
  liftE (rmToolVersion tool tv)
    & runE @'[NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo]
    & flip runReaderT appState
    <&> toOpError "Uninstall failed"

setDefault :: GhcupEnv -> Tool -> TargetVersion -> IO (Either OpError ())
setDefault env tool tv = runIn env $ \appState -> do
  result <-
    liftE (setToolVersion tool tv)
      & runE @'[ParseError, NotInstalled]
      & flip runReaderT appState
  pure (void (toOpError "Could not set default" result))
