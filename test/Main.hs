module Main where

import Test.Tasty

import ConfigSpec qualified
import InstallFormSpec qualified
import PresentationSpec qualified
import SessionSpec qualified
import Toolchain.CurationSpec qualified
import Toolchain.PathSpec qualified
import WorkerSpec qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "ghcup-gtk"
      [ ConfigSpec.tests
      , InstallFormSpec.tests
      , PresentationSpec.tests
      , SessionSpec.tests
      , Toolchain.CurationSpec.tests
      , Toolchain.PathSpec.tests
      , WorkerSpec.tests
      ]
