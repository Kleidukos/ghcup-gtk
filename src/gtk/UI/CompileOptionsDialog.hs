module UI.CompileOptionsDialog
  ( present
  ) where

import Control.Monad (forM_, join, void, when)
import Data.GI.Base
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import GHCup.Types (BuildSystem (..), TargetVersion, TargetVersionReq (..), ghc, hls, tVerToText)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.CompileForm.Ghc
import Presentation.CompileForm.Hls
import Presentation.Row (RowSpec (..), toolShortName)
import Toolchain.Types (Mutation (..))
import UI.Dialog qualified as Dialog

isolateSubtitle :: Text
isolateSubtitle = "Install into ghcup's own directory"

setSubtitle :: Text
setSubtitle = "Make this the active version after the build"

present :: Adw.ApplicationWindow -> RowSpec -> (Mutation -> IO ()) -> IO ()
present window spec onCompile
  | spec.tool == ghc = presentGhc window heading tv (initGhcFormModel spec.installedGhcs) onCompile
  | spec.tool == hls = presentHls window heading tv (initHlsFormModel spec.installedGhcs) onCompile
  | otherwise = pure ()
  where
    TargetVersionReq tv _ = spec.installReq
    heading = "Compile " <> toolShortName spec.tool <> " " <> tVerToText tv <> " from source"

entryRow :: Adw.PreferencesGroup -> Text -> Text -> (Text -> IO ()) -> IO Adw.EntryRow
entryRow group title initial onChanged = do
  row <- new Adw.EntryRow [#title := title, #text := initial]
  void $ on row #changed (row.getText >>= onChanged)
  group.add =<< Gtk.toWidget row
  pure row

markError :: Adw.EntryRow -> Maybe Text -> IO ()
markError row = \case
  Just message -> do
    row.addCssClass "error"
    row.setTooltipText (Just message)
  Nothing -> do
    row.removeCssClass "error"
    row.setTooltipText Nothing

data Scaffold = Scaffold
  { group :: Adw.PreferencesGroup
  , compileButton :: Gtk.Button
  , isolateRow :: Adw.ActionRow
  , clearButton :: Gtk.Button
  , setRow :: Adw.SwitchRow
  , closeDialog :: IO ()
  , presentDialog :: IO ()
  }

scaffold
  :: Adw.ApplicationWindow
  -> Text
  -> (FilePath -> IO ())
  -> IO ()
  -> IO Scaffold
scaffold window heading onIsolatePicked onIsolateCleared = do
  group <- new Adw.PreferencesGroup []

  isolateRow <-
    new
      Adw.ActionRow
      [ #title := "Isolate to directory"
      , #subtitle := isolateSubtitle
      ]
  pickButton <-
    new
      Gtk.Button
      [ #iconName := "folder-open-symbolic"
      , #valign := Gtk.AlignCenter
      , #cssClasses := ["flat"]
      , #tooltipText := "Choose a directory"
      ]
  clearButton <-
    new
      Gtk.Button
      [ #iconName := "edit-clear-symbolic"
      , #valign := Gtk.AlignCenter
      , #cssClasses := ["flat"]
      , #tooltipText := "Do not isolate"
      , #visible := False
      ]
  isolateRow.addSuffix pickButton
  isolateRow.addSuffix clearButton

  setRow <-
    new
      Adw.SwitchRow
      [ #title := "Set as default"
      , #subtitle := setSubtitle
      ]

  clamp <-
    new
      Adw.Clamp
      [ #maximumSize := 480
      , #cssClasses := ["install-options-content"]
      ]
  groupWidget <- Gtk.toWidget group
  clamp.setChild (Just groupWidget)

  scrolled <-
    new
      Gtk.ScrolledWindow
      [ #hscrollbarPolicy := Gtk.PolicyTypeNever
      , #propagateNaturalHeight := True
      ]
  clampWidget <- Gtk.toWidget clamp
  scrolled.setChild (Just clampWidget)

  cancelButton <- new Gtk.Button [#label := "Cancel"]
  compileButton <-
    new Gtk.Button [#label := "Compile", #cssClasses := ["suggested-action"]]

  header <-
    new
      Adw.HeaderBar
      [ #showStartTitleButtons := False
      , #showEndTitleButtons := False
      ]
  header.packStart cancelButton
  header.packEnd compileButton

  toolbarView <- new Adw.ToolbarView []
  headerWidget <- Gtk.toWidget header
  toolbarView.addTopBar headerWidget
  scrolledWidget <- Gtk.toWidget scrolled
  toolbarView.setContent (Just scrolledWidget)

  dialog <-
    new
      Adw.Dialog
      [ #title := heading
      , #contentWidth := 460
      , #contentHeight := 640
      ]
  toolbarWidget <- Gtk.toWidget toolbarView
  dialog.setChild (Just toolbarWidget)

  void $
    on pickButton #clicked $
      Dialog.pickFolder window "Isolate installation to…" onIsolatePicked
  void $ on clearButton #clicked onIsolateCleared
  void $ on cancelButton #clicked dialog.forceClose

  pure
    Scaffold
      { group
      , compileButton
      , isolateRow
      , clearButton
      , setRow
      , closeDialog = dialog.forceClose
      , presentDialog = dialog.present (Just window)
      }

renderIsolate :: Scaffold -> Maybe FilePath -> Bool -> IO ()
renderIsolate ui isolateDir setCompile = do
  case isolateDir of
    Just path -> do
      ui.isolateRow.setSubtitle (Text.pack path)
      ui.clearButton.setVisible True
    Nothing -> do
      ui.isolateRow.setSubtitle isolateSubtitle
      ui.clearButton.setVisible False
  let locked = isJust isolateDir
  ui.setRow.setSensitive (not locked)
  ui.setRow.setSubtitle
    (if locked then "Isolated installs cannot be set as default" else setSubtitle)
  ui.setRow.setActive setCompile

wireSetToggle :: Scaffold -> IO Bool -> (Bool -> IO ()) -> IO ()
wireSetToggle ui getCurrent toggle =
  void $ on ui.setRow (PropertyNotify #active) $ \_ -> do
    active <- ui.setRow.getActive
    current <- getCurrent
    when (active /= current) (toggle active)

wireCompile :: Scaffold -> IO (Either Text Mutation) -> (Mutation -> IO ()) -> IO ()
wireCompile ui getMutation onCompile =
  void $ on ui.compileButton #clicked $ do
    mutation <- getMutation
    forM_ mutation $ \m -> do
      ui.closeDialog
      onCompile m

presentGhc :: Adw.ApplicationWindow -> Text -> TargetVersion -> GhcFormModel -> (Mutation -> IO ()) -> IO ()
presentGhc window heading tv initialModel onCompile = do
  modelRef <- newIORef initialModel
  renderRef <- newIORef (pure ())

  let dispatch event = do
        modifyIORef' modelRef (stepGhcForm event)
        join (readIORef renderRef)

  ui <- scaffold window heading (dispatch . GhcIsolatePicked) (dispatch GhcIsolateCleared)

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
    ui.compileButton.setSensitive (canCompileGhc model)
    renderIsolate ui model.isolateDir model.setCompile

  wireSetToggle ui ((.setCompile) <$> readIORef modelRef) (dispatch . GhcSetToggled)
  wireCompile ui (fmap (CompileGhc tv) . toGhcOptions <$> readIORef modelRef) onCompile

  join (readIORef renderRef)
  ui.presentDialog

presentHls :: Adw.ApplicationWindow -> Text -> TargetVersion -> HlsFormModel -> (Mutation -> IO ()) -> IO ()
presentHls window heading tv initialModel onCompile = do
  modelRef <- newIORef initialModel
  renderRef <- newIORef (pure ())

  let dispatch event = do
        modifyIORef' modelRef (stepHlsForm event)
        join (readIORef renderRef)

  ui <- scaffold window heading (dispatch . HlsIsolatePicked) (dispatch HlsIsolateCleared)

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
  void $ on updateCabalRow (PropertyNotify #active) $ \_ -> do
    active <- updateCabalRow.getActive
    model <- readIORef modelRef
    when (active /= model.updateCabal) $ dispatch (HlsUpdateCabalToggled active)

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
    ui.compileButton.setSensitive (canCompileHls model)
    renderIsolate ui model.isolateDir model.setCompile

  wireSetToggle ui ((.setCompile) <$> readIORef modelRef) (dispatch . HlsSetToggled)
  wireCompile ui (fmap (CompileHls tv) . toHlsOptions <$> readIORef modelRef) onCompile

  join (readIORef renderRef)
  ui.presentDialog
