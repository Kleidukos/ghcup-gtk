module Toolchain.PathSpec (tests) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import System.Directory
  ( createDirectoryIfMissing
  , createFileLink
  , getSymbolicLinkTarget
  , getTemporaryDirectory
  , pathIsSymbolicLink
  , removePathForcibly
  )
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Effects.FileSystem (runFileSystemIO)
import Fixtures (dirs)
import TestInterpreters (runFileSystemPure)
import Toolchain.Path
import Toolchain.Types (GhcupDirs (..), OpError (..))

xdgDirs :: GhcupDirs
xdgDirs = GhcupDirs {ghcupBinDir = "/home/u/.local/bin", ghcupBaseDir = "/home/u/.local/share/ghcup"}

paths :: Vector FileChange -> [FilePath]
paths = map (.path) . Vector.toList

env :: EnvSnapshot
env =
  EnvSnapshot
    { envShell = "/usr/bin/zsh"
    , envPath = "/usr/bin:/bin"
    , envHome = "/home/u"
    , envZdotdir = Nothing
    , envProfileExists = False
    , envDirs = dirs
    }

tests :: TestTree
tests =
  testGroup
    "Path"
    [ testCase "detectShell knows zsh, bash and fish; sh (no $BASH/$ZSH_VERSION in a GUI process) and empty are unknown" $ do
        detectShell "/usr/bin/zsh" @?= Zsh
        detectShell "/bin/bash" @?= Bash
        detectShell "/usr/bin/fish" @?= Fish
        detectShell "/bin/sh" @?= UnknownShell "/bin/sh"
        detectShell "" @?= UnknownShell ""
    , testCase "pathContains matches whole entries only, tolerating a trailing slash" $ do
        pathContains "/home/u/.ghcup/bin" "/usr/bin:/home/u/.ghcup/bin:/bin" @?= True
        pathContains "/home/u/.ghcup/bin" "/usr/bin:/bin" @?= False
        pathContains "/home/u/.ghcup/bin" "/home/u/.ghcup/bin/:/bin" @?= True
        pathContains "/home/u/.ghcup/bin" "/home/u/.ghcup/bin-extra:/bin" @?= False
        pathContains "/home/u/.ghcup/bin" "" @?= False
    , testGroup
        "planFix"
        [ testCase "zsh: env file + .zshrc line" $ do
            changes <- planOf env
            paths changes @?= ["/home/u/.ghcup/env", "/home/u/.zshrc"]
            (envFile, rc) <- pairOf changes
            envFile.mode @?= CreateOrReplace
            rc.mode @?= FilteredAppend
            rc.payload
              @?= "[ -f \"/home/u/.ghcup/env\" ] && . \"/home/u/.ghcup/env\" # ghcup-env"
        , testCase "zsh honors ZDOTDIR" $ do
            changes <- planOf env {envZdotdir = Just "/home/u/cfg"}
            paths changes @?= ["/home/u/.ghcup/env", "/home/u/cfg/.zshrc"]
        , testCase "bash targets .bashrc" $ do
            changes <- planOf env {envShell = "/bin/bash"}
            paths changes @?= ["/home/u/.ghcup/env", "/home/u/.bashrc"]
        , testCase ".profile appended when it exists" $ do
            changes <- planOf env {envProfileExists = True}
            paths changes
              @?= ["/home/u/.ghcup/env", "/home/u/.zshrc", "/home/u/.profile"]
        , testCase "fish writes config.fish, no env source line" $ do
            changes <- planOf env {envShell = "/usr/bin/fish"}
            paths changes
              @?= ["/home/u/.ghcup/env", "/home/u/.config/fish/config.fish"]
            (_, fishRc) <- pairOf changes
            Text.isInfixOf "set -gx PATH $HOME/.cabal/bin /home/u/.ghcup/bin $PATH # ghcup-env" fishRc.payload
              @?= True
        , testCase "unknown shell: no plan" $
            planFix env {envShell = "/bin/sh"} @?= Nothing
        , testCase "XDG dirs: all ghcup paths come from GhcupDirs, not $HOME" $ do
            changes <- planOf env {envDirs = xdgDirs}
            paths changes
              @?= ["/home/u/.local/share/ghcup/env", "/home/u/.zshrc"]
            (_, rc) <- pairOf changes
            rc.payload
              @?= "[ -f \"/home/u/.local/share/ghcup/env\" ] && . \"/home/u/.local/share/ghcup/env\" # ghcup-env"
        ]
    , testGroup
        "envFileContent"
        [ testCase "case-guarded prepend, script variant 1" $ do
            let body = envFileContent dirs
            Text.isInfixOf "case \":$PATH:\" in" body @?= True
            Text.isInfixOf "export PATH=\"/home/u/.ghcup/bin:$PATH\"" body @?= True
            Text.isInfixOf "export PATH=\"$HOME/.cabal/bin:$PATH\"" body @?= True
        , testCase "XDG dirs: prepends the resolved bin dir" $
            Text.isInfixOf
              "export PATH=\"/home/u/.local/bin:$PATH\""
              (envFileContent xdgDirs)
              @?= True
        ]
    , testGroup
        "filterMarker"
        [ testCase "drops marked lines, keeps the rest" $
            filterMarker "keep\nsource x # ghcup-env\nkeep2\n" @?= "keep\nkeep2\n"
        , testCase "idempotent on clean input" $
            filterMarker "a\nb\n" @?= "a\nb\n"
        , testCase "normalizes a missing trailing newline (pinned behavior)" $
            filterMarker "a" @?= "a\n"
        , testCase "filter-then-append composition is idempotent (applyFix invariant)" $ do
            let line = "source env # ghcup-env"
                appendOnce t = filterMarker t <> line <> "\n"
                orig = "keep\nold line # ghcup-env\nkeep2\n"
            appendOnce (appendOnce orig) @?= appendOnce orig
        ]
    , testGroup
        "checkPath (pure interpreters)"
        [ testCase "bin dir on PATH → PathOk" $ do
            let vars = Map.fromList [("SHELL", "/bin/bash"), ("PATH", "/usr/bin:/home/u/.ghcup/bin")]
            fst (runCheck vars Map.empty) @?= PathOk
        , testCase "marker already in rc → FixedAwaitingRestart, trailing whitespace tolerated" $ do
            let vars = Map.fromList [("SHELL", "/bin/bash"), ("PATH", "/usr/bin")]
                files = Map.singleton "/fake/home/.bashrc" "source env # ghcup-env\n"
            fst (runCheck vars files) @?= FixedAwaitingRestart
            let trailing = Map.singleton "/fake/home/.bashrc" "source env # ghcup-env  \n"
            fst (runCheck vars trailing) @?= FixedAwaitingRestart
        , testCase "bash without marker → NeedsFixPlanned" $ do
            let vars = Map.fromList [("SHELL", "/bin/bash"), ("PATH", "/usr/bin")]
            case fst (runCheck vars Map.empty) of
              NeedsFixPlanned changes ->
                paths changes @?= ["/home/u/.ghcup/env", "/fake/home/.bashrc"]
              other -> assertFailure ("expected NeedsFixPlanned, got: " <> show other)
        , testCase "unknown shell → NeedsFixManual" $ do
            let vars = Map.fromList [("SHELL", "/bin/sh"), ("PATH", "/usr/bin")]
            fst (runCheck vars Map.empty) @?= NeedsFixManual
        , testCase "marker as infix of a benign line is not a fix (matches filterMarker)" $ do
            let vars = Map.fromList [("SHELL", "/bin/bash"), ("PATH", "/usr/bin")]
                files = Map.singleton "/fake/home/.bashrc" "## ghcup-environment tweaks\nalias g=ghcup\n"
            case fst (runCheck vars files) of
              NeedsFixPlanned _ -> pure ()
              other -> assertFailure ("expected NeedsFixPlanned, got: " <> show other)
        ]
    , testGroup
        "applyFix (pure interpreters)"
        [ testCase "CreateOrReplace writes the payload" $ do
            let (result, files) =
                  runApply Map.empty [] (change "/fake/home/.ghcup/env" "content" CreateOrReplace)
            result @?= Right ()
            Map.lookup "/fake/home/.ghcup/env" files @?= Just "content"
        , testCase "FilteredAppend on a fresh file appends the line" $ do
            let (result, files) =
                  runApply Map.empty [] (change "/fake/home/.bashrc" "source env # ghcup-env" FilteredAppend)
            result @?= Right ()
            Map.lookup "/fake/home/.bashrc" files @?= Just "source env # ghcup-env\n"
        , testCase "FilteredAppend drops stale marker lines before appending" $ do
            let existing = Map.singleton "/fake/home/.bashrc" "keep\nold # ghcup-env\n"
                (result, files) =
                  runApply existing [] (change "/fake/home/.bashrc" "new # ghcup-env" FilteredAppend)
            result @?= Right ()
            Map.lookup "/fake/home/.bashrc" files @?= Just "keep\nnew # ghcup-env\n"
        , testCase "an unreadable rc aborts the fix and clobbers nothing" $ do
            let (result, files) =
                  runApply Map.empty ["/fake/home/.bashrc"] (change "/fake/home/.bashrc" "line" FilteredAppend)
            case result of
              Left err -> err.title @?= "Could not update shell configuration"
              Right () -> assertFailure "expected the fix to abort"
            Map.member "/fake/home/.bashrc" files @?= False
        ]
    , testGroup
        "applyFix (live filesystem)"
        [ testCase "edits through a symlinked rc file without replacing the link" $ do
            tmpRoot <- getTemporaryDirectory
            let dir = tmpRoot </> "ghcup-gtk-pathspec"
                target = dir </> "dotfiles/zshrc"
                link = dir </> ".zshrc"
                fix = runEff . runFileSystemIO . applyFix
            removePathForcibly dir
            createDirectoryIfMissing True (dir </> "dotfiles")
            Text.writeFile target "# my config\n"
            createFileLink target link
            Right () <-
              fix (Vector.singleton (FileChange link "source env # ghcup-env" FilteredAppend))

            pathIsSymbolicLink link >>= assertBool ".zshrc is still a symlink"
            getSymbolicLinkTarget link >>= (@?= target)

            Right () <-
              fix (Vector.singleton (FileChange link "source env # ghcup-env" FilteredAppend))
            content <- Text.readFile target
            content @?= "# my config\nsource env # ghcup-env\n"
            removePathForcibly dir
        ]
    ]

planOf :: EnvSnapshot -> IO (Vector FileChange)
planOf e = maybe (assertFailure "expected a fix plan") pure (planFix e)

pairOf :: (Show a) => Vector a -> IO (a, a)
pairOf v = case Vector.toList v of
  [a, b] -> pure (a, b)
  other -> assertFailure ("expected exactly two elements, got: " <> show other)

runCheck :: Map String String -> Map FilePath Text -> (PathStatus, Map FilePath Text)
runCheck vars files =
  runPureEff (runFileSystemPure vars files [] (checkPath dirs))

runApply
  :: Map FilePath Text
  -> [FilePath]
  -> FileChange
  -> (Either OpError (), Map FilePath Text)
runApply files unreadable c =
  runPureEff (runFileSystemPure Map.empty files unreadable (applyFix (Vector.singleton c)))

change :: FilePath -> Text -> WriteMode -> FileChange
change path payload mode = FileChange {path, payload, mode}
