module UI.CompileOptionsDialog
  ( present
  ) where

import Control.Monad (join, void)
import Data.GI.Base
import Data.IORef
import Data.Text (Text)
import Data.Versions (Version)
import GHCup.Types (BuildSystem (..), TargetVersionReq (..), ghc, hls, tVerToText)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.CompileForm.Ghc
import Presentation.CompileForm.Hls
import Presentation.Row (RowSpec (..), compileGhcMutation, compileHlsMutation, toolShortName)
import Toolchain.Types (CompileGhcOptions, CompileHlsOptions, Mutation)
import UI.OptionsDialog

setSubtitle :: Text
setSubtitle = "Make this the active version after the build"

present :: Adw.ApplicationWindow -> [Version] -> RowSpec -> (Mutation -> IO ()) -> IO ()
present window installedGhcs spec onCompile
  | spec.tool == ghc = presentGhc window heading (compileGhcMutation spec) (initGhcFormModel installedGhcs) onCompile
  | spec.tool == hls = presentHls window heading (compileHlsMutation spec) (initHlsFormModel installedGhcs) onCompile
  | otherwise = pure ()
  where
    TargetVersionReq tv _ = spec.installReq
    heading = "Compile " <> toolShortName spec.tool <> " " <> tVerToText tv <> " from source"

presentGhc :: Adw.ApplicationWindow -> Text -> (CompileGhcOptions -> Mutation) -> GhcFormModel -> (Mutation -> IO ()) -> IO ()
presentGhc window heading mkMutation initialModel onCompile = do
  modelRef <- newIORef initialModel
  renderRef <- newIORef (pure ())

  let dispatch event = do
        modifyIORef' modelRef (stepGhcForm event)
        join (readIORef renderRef)

  ui <-
    scaffold
      window
      ScaffoldSpec
        { heading
        , affirmLabel = "Compile"
        , setSubtitle
        , contentHeight = Just 640
        , onIsolatePicked = dispatch . GhcIsolatePicked
        , onIsolateCleared = dispatch GhcIsolateCleared
        }

  bootstrapRow <-
    entryRow ui.group "Bootstrap GHC (version or absolute path)" initialModel.bootstrapGhc (dispatch . GhcBootstrapChanged)
  hadrianRow <-
    entryRow ui.group "Hadrian GHC (version or absolute path)" "" (dispatch . GhcHadrianChanged)
  jobsRow <- entryRow ui.group "Jobs" "" (dispatch . GhcJobsChanged)
  void $ entryRow ui.group "Build flavour" "" (dispatch . GhcFlavourChanged)

  buildSystems <- Gtk.stringListNew (Just ["Automatic", "Hadrian", "Make"])
  buildSystemRow <-
    new
      Adw.ComboRow
      [ #title := "Build system"
      , #model := buildSystems
      ]
  ui.group.add =<< Gtk.toWidget buildSystemRow
  void $ on buildSystemRow (PropertyNotify #selected) $ \_ -> do
    selected <- buildSystemRow.getSelected
    dispatch $ GhcBuildSystemChanged $ case selected of
      1 -> Just Hadrian
      2 -> Just Make
      _ -> Nothing

  buildConfigRow <-
    entryRow ui.group "Build config file (make only)" "" (dispatch . GhcBuildConfigChanged)
  void $
    entryRow ui.group "Extra configure args" "" (dispatch . GhcConfArgsChanged)
  patchesRow <-
    entryRow ui.group "Patches (directory or URLs)" "" (dispatch . GhcPatchesChanged)
  void $ entryRow ui.group "Cross target" "" (dispatch . GhcCrossTargetChanged)
  overwriteRow <-
    entryRow ui.group "Overwrite version (%v, %b, %h, %H, %g)" "" (dispatch . GhcOverwriteChanged)
  void $
    entryRow ui.group "Docs (e.g. none, no-sphinx)" "" (dispatch . GhcDocsChanged)
  void $
    entryRow ui.group "Install targets" "" (dispatch . GhcInstallTargetsChanged)
  void $
    entryRow ui.group "Git ref (build from git instead of the release)" "" (dispatch . GhcGitRefChanged)

  ui.group.add =<< Gtk.toWidget ui.isolateRow
  ui.group.add =<< Gtk.toWidget ui.setRow

  writeIORef renderRef $ do
    model <- readIORef modelRef
    markError bootstrapRow (ghcFieldError model GhcBootstrapField)
    markError hadrianRow (ghcFieldError model GhcHadrianField)
    markError jobsRow (ghcFieldError model GhcJobsField)
    markError buildConfigRow (ghcFieldError model GhcBuildConfigField)
    markError patchesRow (ghcFieldError model GhcPatchesField)
    markError overwriteRow (ghcFieldError model GhcOverwriteField)
    ui.affirmButton.setSensitive (canCompileGhc model)
    renderIsolate ui model.isolateDir (effectiveSetCompileGhc model)

  wireSetToggle ui (effectiveSetCompileGhc <$> readIORef modelRef) (dispatch . GhcSetToggled)
  wireAffirm ui (fmap mkMutation . toGhcOptions <$> readIORef modelRef) onCompile

  join (readIORef renderRef)
  ui.presentDialog

presentHls :: Adw.ApplicationWindow -> Text -> (CompileHlsOptions -> Mutation) -> HlsFormModel -> (Mutation -> IO ()) -> IO ()
presentHls window heading mkMutation initialModel onCompile = do
  modelRef <- newIORef initialModel
  renderRef <- newIORef (pure ())

  let dispatch event = do
        modifyIORef' modelRef (stepHlsForm event)
        join (readIORef renderRef)

  ui <-
    scaffold
      window
      ScaffoldSpec
        { heading
        , affirmLabel = "Compile"
        , setSubtitle
        , contentHeight = Just 640
        , onIsolatePicked = dispatch . HlsIsolatePicked
        , onIsolateCleared = dispatch HlsIsolateCleared
        }

  targetGhcsRow <-
    entryRow ui.group "Target GHC versions" initialModel.targetGhcs (dispatch . HlsTargetGhcsChanged)
  jobsRow <- entryRow ui.group "Jobs" "" (dispatch . HlsJobsChanged)

  updateCabalRow <-
    new
      Adw.SwitchRow
      [ #title := "Run cabal update"
      , #subtitle := "Refresh the package index before the build"
      ]
  ui.group.add =<< Gtk.toWidget updateCabalRow
  wireSwitch updateCabalRow ((.updateCabal) <$> readIORef modelRef) (dispatch . HlsUpdateCabalToggled)

  void $
    entryRow ui.group "Extra cabal install args" "" (dispatch . HlsCabalArgsChanged)
  cabalProjectRow <-
    entryRow ui.group "cabal.project (path in the source tree, or URL)" "" (dispatch . HlsCabalProjectChanged)
  cabalProjectLocalRow <-
    entryRow ui.group "cabal.project.local URL" "" (dispatch . HlsCabalProjectLocalChanged)
  patchesRow <-
    entryRow ui.group "Patches (directory or URLs)" "" (dispatch . HlsPatchesChanged)
  overwriteRow <-
    entryRow ui.group "Overwrite version (%v, %b, %h, %H, %g)" "" (dispatch . HlsOverwriteChanged)
  void $
    entryRow ui.group "Git ref (build from git instead of the release)" "" (dispatch . HlsGitRefChanged)

  ui.group.add =<< Gtk.toWidget ui.isolateRow
  ui.group.add =<< Gtk.toWidget ui.setRow

  writeIORef renderRef $ do
    model <- readIORef modelRef
    markError targetGhcsRow (hlsFieldError model HlsTargetGhcsField)
    markError jobsRow (hlsFieldError model HlsJobsField)
    markError cabalProjectRow (hlsFieldError model HlsCabalProjectField)
    markError cabalProjectLocalRow (hlsFieldError model HlsCabalProjectLocalField)
    markError patchesRow (hlsFieldError model HlsPatchesField)
    markError overwriteRow (hlsFieldError model HlsOverwriteField)
    ui.affirmButton.setSensitive (canCompileHls model)
    renderIsolate ui model.isolateDir (effectiveSetCompileHls model)

  wireSetToggle ui (effectiveSetCompileHls <$> readIORef modelRef) (dispatch . HlsSetToggled)
  wireAffirm ui (fmap mkMutation . toHlsOptions <$> readIORef modelRef) onCompile

  join (readIORef renderRef)
  ui.presentDialog
