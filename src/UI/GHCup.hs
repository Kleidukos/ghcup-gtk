module UI.GHCup
  ( initAppState
  , module GHCup.Types -- largely for instances
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text.IO qualified as Text
import GHCup.Download (getDownloadsF)
import GHCup.Errors
import GHCup.Platform
import GHCup.Prelude (hideError)
import GHCup.Prelude.File (findFiles, recycleFile)
import GHCup.Prelude.Logger (logError)
import GHCup.Prelude.String.QQ (s)
import GHCup.Types
import GHCup.Types qualified as Types
import GHCup.Types.Optics (getDirs)
import GHCup.Utils (ghcupConfigFile)
import GHCup.Utils.Dirs (fromGHCupPath, getAllDirs)
import Haskus.Utils.Variant.Excepts (liftE, runE)
import Haskus.Utils.Variant.VEither
import System.FilePath ((</>))
import System.IO.Error (doesNotExistErrorType)
import Text.Regex.Posix (compExtended, execBlank, makeRegexOpts)

-- adapted from GHCup CLI source
getSettings :: IO (Settings, KeyBindings, UserSettings)
getSettings = do
  userConf <-
    runE @'[JSONError] ghcupConfigFile >>= \case
      VRight r -> pure r
      VLeft (V (JSONDecodeError e)) -> do
        Text.putStrLn $ "Error decoding config file: " <> (pack . show $ e)
        pure defaultUserSettings
      _ -> error "toSettings: Unexpected error!"
  pure $ (\(s', k) -> (s', k, userConf)) $ mergeConf userConf
  where
    mergeConf :: UserSettings -> (Settings, KeyBindings)
    mergeConf UserSettings{..} =
      let cache = fromMaybe (Types.cache defaultSettings) uCache
          metaCache = fromMaybe (Types.metaCache defaultSettings) uMetaCache
          metaMode = fromMaybe (Types.metaMode defaultSettings) uMetaMode
          noVerify = fromMaybe (Types.noVerify defaultSettings) uNoVerify
          verbose = fromMaybe (Types.verbose defaultSettings) uVerbose
          keepDirs = fromMaybe (Types.keepDirs defaultSettings) uKeepDirs
          downloader = fromMaybe defaultDownloader uDownloader
          keyBindings = maybe defaultKeyBindings mergeKeys uKeyBindings
          urlSource = fromMaybe (Types.urlSource defaultSettings) uUrlSource
          noNetwork = fromMaybe (Types.noNetwork defaultSettings) uNoNetwork
          gpgSetting = fromMaybe (Types.gpgSetting defaultSettings) uGPGSetting
          platformOverride = uPlatformOverride <|> Types.platformOverride defaultSettings
          mirrors = fromMaybe (Types.mirrors defaultSettings) uMirrors
          noColor = True
       in (Settings{..}, keyBindings)
    -- #if defined(INTERNAL_DOWNLOADER)
    --    defaultDownloader = Internal
    -- #else
    defaultDownloader = Curl
    -- #endif
    mergeKeys :: UserKeyBindings -> KeyBindings
    mergeKeys UserKeyBindings{..} =
      let KeyBindings{..} = defaultKeyBindings
       in KeyBindings
            { bUp = fromMaybe bUp kUp
            , bDown = fromMaybe bDown kDown
            , bQuit = fromMaybe bQuit kQuit
            , bInstall = fromMaybe bInstall kInstall
            , bUninstall = fromMaybe bUninstall kUninstall
            , bSet = fromMaybe bSet kSet
            , bChangelog = fromMaybe bChangelog kChangelog
            , bShowAllVersions = fromMaybe bShowAllVersions kShowAll
            , bShowAllTools = fromMaybe bShowAllTools kShowAllTools
            }

-- adapted from 'initGHCupFileLogging'
-- with different file name
initGHCupGTKFileLogging :: ReaderT Dirs IO FilePath
initGHCupGTKFileLogging = do
  Dirs{logsDir} <- getDirs
  let logfile = fromGHCupPath logsDir </> "ghcup-gtk.log"
  logFiles <-
    liftIO $
      findFiles
        (fromGHCupPath logsDir)
        ( makeRegexOpts
            compExtended
            execBlank
            ([s|^.*\.log$|] :: ByteString)
        )
  forM_ logFiles $ hideError doesNotExistErrorType . recycleFile . (fromGHCupPath logsDir </>)

  liftIO $ writeFile logfile ""
  pure logfile

initAppState :: IO (Either Text AppState)
initAppState = do
  dirs <- getAllDirs
  logfile <- runReaderT initGHCupGTKFileLogging dirs
  (settings, keybinds, _userSettings) <- getSettings
  let loggerConfig =
        LoggerConfig
          { lcPrintDebug = False
          , consoleOutter = Text.putStrLn
          , fileOutter = Text.appendFile logfile
          , fancyColors = False
          }
      leanAppstate = LeanAppState settings dirs keybinds loggerConfig

      runLogger :: ReaderT LeanAppState m a -> m a -- need type signature!
      runLogger = flip runReaderT leanAppstate

  pfreq' <-
    runLogger
      . runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound]
      . liftE
      $ platformRequest
  shortCircuit runLogger pfreq' $ \pfreq -> do
    ghcupInfo' <-
      runLogger
        . runE @'[DigestError, ContentLengthError, GPGError, JSONError, DownloadFailed, FileDoesNotExistError]
        $ liftE getDownloadsF
    shortCircuit runLogger ghcupInfo' $ \ghcupInfo ->
      pure $
        Right $
          AppState settings dirs keybinds ghcupInfo pfreq loggerConfig
  where
    shortCircuit _ (VRight v) m = m v
    shortCircuit runLogger (VLeft e) _ = do
      let e' = pack $ prettyHFError e
      runLogger $ logError e'
      pure $ Left e'
