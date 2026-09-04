module Main where

import Test.Tasty

import CompileFormSpec qualified
import ConfigSpec qualified
import FilterSpec qualified
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
      [ CompileFormSpec.tests
      , ConfigSpec.tests
      , FilterSpec.tests
      , InstallFormSpec.tests
      , PresentationSpec.tests
      , SessionSpec.tests
      , Toolchain.CurationSpec.tests
      , Toolchain.PathSpec.tests
      , WorkerSpec.tests
      ]
