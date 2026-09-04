module Fixtures
  ( mkLR
  , lr914
  , dirs
  , anError
  , installMutation
  , installJob
  , listingsFor
  , sampleChanges
  , defaultCompileGhcOptions
  , defaultCompileHlsOptions
  , defaultNightliesUri
  , uriOf
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Versions (Version, version)
import GHCup.Command.List (ListResult (..), RevTag (..))
import GHCup.Types (Tag (..), Tool, ghc)
import URI.ByteString (URI, parseURI, strictURIParserOptions)

import Toolchain.Channels (defaultNightliesUrl, parseNightlies)
import Toolchain.Path (FileChange (..), WriteMode (..))
import Toolchain.Types

mkLR :: Text -> [Tag] -> Bool -> Bool -> ListResult
mkLR v tags inst dflt =
  ListResult
    { lVer = either (error . show) id (version v)
    , lCross = Nothing
    , lRev = (0, RevNormal)
    , lTag = tags
    , lInstalled = inst
    , lSet = dflt
    , lStray = False
    , lNoBindist = False
    , hlsPowered = False
    , lReleaseDay = Nothing
    }

lr914 :: ListResult
lr914 = mkLR "9.14.1" [Latest] False False

dirs :: GhcupDirs
dirs = GhcupDirs {ghcupBinDir = "/home/u/.ghcup/bin", ghcupBaseDir = "/home/u/.ghcup"}

anError :: OpError
anError = OpError "boom" "details"

installMutation :: Mutation
installMutation = Install ghc (reqOf lr914) defaultInstallOptions

installJob :: Job
installJob = Mutate installMutation

listingsFor :: Tool -> [ListResult] -> Listings
listingsFor tool = Map.singleton tool . Vector.fromList

defaultCompileGhcOptions :: Either Version FilePath -> CompileGhcOptions
defaultCompileGhcOptions bootstrapGhc =
  CompileGhcOptions
    { bootstrapGhc
    , hadrianGhc = Nothing
    , jobs = Nothing
    , buildConfig = Nothing
    , patches = Nothing
    , crossTarget = Nothing
    , addConfArgs = []
    , setCompile = False
    , overwriteVer = Nothing
    , buildFlavour = Nothing
    , buildSystem = Nothing
    , isolateDir = Nothing
    , gitRef = Nothing
    , installTargets = Nothing
    , docs = Nothing
    }

defaultCompileHlsOptions :: [Version] -> CompileHlsOptions
defaultCompileHlsOptions targetGhcs =
  CompileHlsOptions
    { targetGhcs
    , jobs = Nothing
    , setCompile = False
    , updateCabal = False
    , overwriteVer = Nothing
    , isolateDir = Nothing
    , cabalProject = Nothing
    , cabalProjectLocal = Nothing
    , patches = Nothing
    , cabalArgs = []
    , gitRef = Nothing
    }

sampleChanges :: Vector FileChange
sampleChanges =
  Vector.fromList
    [ FileChange "/home/u/.ghcup/env" "export PATH=..." CreateOrReplace
    , FileChange "/home/u/.zshrc" "source env # ghcup-env" FilteredAppend
    ]

uriOf :: ByteString -> URI
uriOf raw =
  case parseURI strictURIParserOptions raw of
    Right uri -> uri
    Left err -> error (show err)

defaultNightliesUri :: URI
defaultNightliesUri =
  case parseNightlies defaultNightliesUrl of
    Just uri -> uri
    Nothing -> error "the default nightlies URL must parse"
