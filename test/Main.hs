module Main where

import Test.Tasty

import CompileFormSpec qualified
import FilterSpec qualified
import InstallFormSpec qualified
import PresentationSpec qualified
import SessionSpec qualified
import Toolchain.ChannelsSpec qualified
import Toolchain.CurationSpec qualified
import Toolchain.GHCupConfigSpec qualified
import Toolchain.PathSpec qualified
import WorkerSpec qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "ghcup-gtk"
      [ CompileFormSpec.tests
      , FilterSpec.tests
      , InstallFormSpec.tests
      , PresentationSpec.tests
      , SessionSpec.tests
      , Toolchain.ChannelsSpec.tests
      , Toolchain.CurationSpec.tests
      , Toolchain.GHCupConfigSpec.tests
      , Toolchain.PathSpec.tests
      , WorkerSpec.tests
      ]
