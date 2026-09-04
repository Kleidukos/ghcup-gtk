module Toolchain.Path
  ( Shell (..)
  , WriteMode (..)
  , FileChange (..)
  , EnvSnapshot (..)
  , PathStatus (..)
  , detectShell
  , pathContains
  , envFileContent
  , sourceLine
  , planFix
  , filterMarker
  , checkPath
  , applyFix
  ) where

import Control.Monad (mfilter)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import System.FilePath (equalFilePath, splitSearchPath, (</>))

import Effects.FileSystem (FileSystem, doesFileExist, getHomeDirectory, lookupEnv, readFileText, writeFileAtomic)
import Effects.HostEnv (HostEnv, getHostEnvironment)
import Toolchain.Types (GhcupDirs (..), OpError (..))

data Shell = Bash | Zsh | Fish | UnknownShell Text
  deriving stock (Eq, Show)

data WriteMode = CreateOrReplace | FilteredAppend
  deriving stock (Eq, Show)

data FileChange = FileChange
  { path :: FilePath
  , payload :: Text
  , mode :: WriteMode
  }
  deriving stock (Eq, Show)

data EnvSnapshot = EnvSnapshot
  { envShell :: Text
  , envPath :: Text
  , envHome :: FilePath
  , envZdotdir :: Maybe FilePath
  , envProfileExists :: Bool
  , envDirs :: GhcupDirs
  }
  deriving stock (Eq, Show)

marker :: Text
marker = "# ghcup-env"

-- | The only non-rc file that a fix can touch. Planning, checking, and
-- snapshotting must agree on it.
profilePath :: FilePath -> FilePath
profilePath home = home </> ".profile"

detectShell :: Text -> Shell
detectShell shellPath
  | "/zsh" `Text.isSuffixOf` shellPath = Zsh
  | "/bash" `Text.isSuffixOf` shellPath = Bash
  | "/fish" `Text.isSuffixOf` shellPath = Fish
  | otherwise = UnknownShell shellPath

pathContains :: FilePath -> Text -> Bool
pathContains dir path =
  any (equalFilePath dir) (splitSearchPath (Text.unpack path))

envFileContent :: GhcupDirs -> Text
envFileContent dirs =
  Text.unlines
    [ "case \":$PATH:\" in"
    , "    *:\"" <> ghcupBin <> "\":*)"
    , "        ;;"
    , "    *)"
    , "        export PATH=\"" <> ghcupBin <> ":$PATH\""
    , "        ;;"
    , "esac"
    , "case \":$PATH:\" in"
    , "    *:\"$HOME/.cabal/bin\":*)"
    , "        ;;"
    , "    *)"
    , "        export PATH=\"$HOME/.cabal/bin:$PATH\""
    , "        ;;"
    , "esac"
    ]
  where
    ghcupBin = Text.pack dirs.ghcupBinDir

envFilePath :: GhcupDirs -> FilePath
envFilePath dirs = dirs.ghcupBaseDir </> "env"

sourceLine :: GhcupDirs -> Text
sourceLine dirs =
  let envFile = Text.pack (envFilePath dirs)
  in "[ -f \"" <> envFile <> "\" ] && . \"" <> envFile <> "\" " <> marker

planFix :: EnvSnapshot -> Maybe (Vector FileChange)
planFix env = do
  rcChange <- rcFor (detectShell env.envShell)
  pure . Vector.fromList $
    [FileChange (envFilePath env.envDirs) (envFileContent env.envDirs) CreateOrReplace]
      <> [rcChange]
      <> [ FileChange (profilePath env.envHome) (sourceLine env.envDirs) FilteredAppend
         | env.envProfileExists
         ]
  where
    rcFor = \case
      Zsh ->
        let dir = fromMaybe env.envHome env.envZdotdir
        in Just (FileChange (dir </> ".zshrc") (sourceLine env.envDirs) FilteredAppend)
      Bash ->
        Just (FileChange (env.envHome </> ".bashrc") (sourceLine env.envDirs) FilteredAppend)
      Fish ->
        Just
          ( FileChange
              (env.envHome </> ".config/fish/config.fish")
              ( "set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; "
                  <> "set -gx PATH $HOME/.cabal/bin "
                  <> Text.pack env.envDirs.ghcupBinDir
                  <> " $PATH "
                  <> marker
              )
              FilteredAppend
          )
      UnknownShell _ -> Nothing

isMarkerLine :: Text -> Bool
isMarkerLine = (marker `Text.isSuffixOf`) . Text.stripEnd

filterMarker :: Text -> Text
filterMarker content = Text.lines content & filter (not . isMarkerLine) & Text.unlines

data PathStatus
  = PathOk
  | FixedAwaitingRestart
  | -- | The shell is unknown. We can only give instructions.
    NeedsFixManual
  | -- | We know exactly which files to change.
    NeedsFixPlanned (Vector FileChange)
  deriving stock (Eq, Show)

snapshotEnvironment
  :: (FileSystem :> es, HostEnv :> es)
  => GhcupDirs
  -> Eff es EnvSnapshot
snapshotEnvironment envDirs = do
  localShell <- maybe "" Text.pack <$> lookupEnv "SHELL"
  localPath <- maybe "" Text.pack <$> lookupEnv "PATH"
  (hostShell, hostPath) <- getHostEnvironment
  let envShell = fromMaybe localShell hostShell
      envPath = fromMaybe localPath hostPath
  envHome <- getHomeDirectory

  envZdotdir <- mfilter (not . null) <$> lookupEnv "ZDOTDIR"
  envProfileExists <- doesFileExist (profilePath envHome)
  pure EnvSnapshot {envShell, envPath, envHome, envZdotdir, envProfileExists, envDirs}

checkPath
  :: (FileSystem :> es, HostEnv :> es)
  => GhcupDirs
  -> Eff es PathStatus
checkPath dirs = do
  env <- snapshotEnvironment dirs
  let ghcupBin = dirs.ghcupBinDir
  if pathContains ghcupBin env.envPath
    then pure PathOk
    else do
      let plan = planFix env
          candidates = case plan of
            Just changes ->
              Vector.toList
                (Vector.map (.path) (Vector.filter (\c -> c.mode == FilteredAppend) changes))
            Nothing -> [profilePath env.envHome]
      markerPresent <-
        or <$> traverse (\candidate -> readFileOrEmpty candidate <&> Text.lines <&> any isMarkerLine) candidates
      pure $
        if markerPresent
          then FixedAwaitingRestart
          else maybe NeedsFixManual NeedsFixPlanned plan

applyFix :: (FileSystem :> es) => Vector FileChange -> Eff es (Either OpError ())
applyFix changes = go (Vector.toList changes)
  where
    go = \case
      [] -> pure (Right ())
      c : rest ->
        apply c >>= \case
          Left e -> pure (Left (OpError "Could not update shell configuration" e))
          Right () -> go rest

    apply c = case c.mode of
      CreateOrReplace -> writeFileAtomic c.path c.payload
      FilteredAppend ->
        readExisting c.path >>= \case
          Left e -> pure (Left e)
          Right existing ->
            writeFileAtomic c.path (filterMarker existing <> c.payload <> "\n")

readExisting :: (FileSystem :> es) => FilePath -> Eff es (Either Text Text)
readExisting p =
  doesFileExist p >>= \case
    False -> pure (Right "")
    True -> readFileText p

readFileOrEmpty :: (FileSystem :> es) => FilePath -> Eff es Text
readFileOrEmpty p = fromRight "" <$> readExisting p
