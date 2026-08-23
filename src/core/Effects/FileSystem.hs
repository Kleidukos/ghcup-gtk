-- | Custom effect for domain-specific operations.
-- Need to be replaced by stock "Effectful.Filesystem" when we have a
-- pure interpreter for it.
module Effects.FileSystem
  ( FileSystem (..)
  , doesFileExist
  , readFileText
  , writeFileAtomic
  , getXdgDirectory
  , getHomeDirectory
  , lookupEnv
  , runFileSystemIO
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic
import System.Directory (XdgDirectory)
import System.Directory qualified as Directory
import System.Environment qualified as System
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openTempFile)

data FileSystem :: Effect where
  DoesFileExist :: FilePath -> FileSystem m Bool
  ReadFileText :: FilePath -> FileSystem m (Either Text Text)
  WriteFileAtomic :: FilePath -> Text -> FileSystem m (Either Text ())
  GetXdgDirectory :: XdgDirectory -> FilePath -> FileSystem m FilePath
  GetHomeDirectory :: FileSystem m FilePath
  LookupEnv :: String -> FileSystem m (Maybe String)

type instance DispatchOf FileSystem = Dynamic

doesFileExist :: (FileSystem :> es) => FilePath -> Eff es Bool
doesFileExist = send . DoesFileExist

readFileText :: (FileSystem :> es) => FilePath -> Eff es (Either Text Text)
readFileText = send . ReadFileText

writeFileAtomic :: (FileSystem :> es) => FilePath -> Text -> Eff es (Either Text ())
writeFileAtomic path payload = send (WriteFileAtomic path payload)

getXdgDirectory :: (FileSystem :> es) => XdgDirectory -> FilePath -> Eff es FilePath
getXdgDirectory which sub = send (GetXdgDirectory which sub)

getHomeDirectory :: (FileSystem :> es) => Eff es FilePath
getHomeDirectory = send GetHomeDirectory

lookupEnv :: (FileSystem :> es) => String -> Eff es (Maybe String)
lookupEnv = send . LookupEnv

runFileSystemIO :: (IOE :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  DoesFileExist path -> liftIO (Directory.doesFileExist path)
  ReadFileText path -> liftIO (tryText (Text.readFile path))
  WriteFileAtomic path payload -> liftIO (tryText (atomicWrite path payload))
  GetXdgDirectory which sub -> liftIO (Directory.getXdgDirectory which sub)
  GetHomeDirectory -> liftIO Directory.getHomeDirectory
  LookupEnv name -> liftIO (System.lookupEnv name)

tryText :: IO a -> IO (Either Text a)
tryText action = first (Text.pack . show) <$> try @SomeException action

atomicWrite :: FilePath -> Text -> IO ()
atomicWrite path payload = do
  Directory.createDirectoryIfMissing True (takeDirectory path)
  exists <- Directory.doesFileExist path
  target <- if exists then Directory.canonicalizePath path else pure path
  (tmp, h) <- openTempFile (takeDirectory target) (takeFileName target <> ".tmp")
  Text.hPutStr h payload
  hClose h
  when exists $ Directory.copyPermissions target tmp
  Directory.renameFile tmp target
