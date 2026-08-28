module Fixtures
  ( mkLR
  , lr914
  , dirs
  , anError
  , installMutation
  , installJob
  , listingsFor
  , sampleChanges
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Versions (version)
import GHCup.Command.List (ListResult (..), RevTag (..))
import GHCup.Types (Tag (..), Tool, ghc)

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
installMutation = Install ghc (reqOf lr914)

installJob :: Job
installJob = Mutate installMutation

listingsFor :: Tool -> [ListResult] -> Listings
listingsFor tool = Map.singleton tool . Vector.fromList

sampleChanges :: Vector FileChange
sampleChanges =
  Vector.fromList
    [ FileChange "/home/u/.ghcup/env" "export PATH=..." CreateOrReplace
    , FileChange "/home/u/.zshrc" "source env # ghcup-env" FilteredAppend
    ]
