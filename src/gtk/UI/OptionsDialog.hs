module UI.OptionsDialog
  ( Scaffold (..)
  , ScaffoldSpec (..)
  , scaffold
  , renderIsolate
  , wireSwitch
  , wireSetToggle
  , wireAffirm
  , entryRow
  , markError
  ) where

import Control.Monad (forM_, void, when)
import Data.GI.Base
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import UI.Dialog qualified as Dialog

isolateSubtitle :: Text
isolateSubtitle = "Install into ghcup's own directory"

data ScaffoldSpec = ScaffoldSpec
  { heading :: Text
  , affirmLabel :: Text
  , setSubtitle :: Text
  , contentHeight :: Maybe Int32
  , onIsolatePicked :: FilePath -> IO ()
  , onIsolateCleared :: IO ()
  }

data Scaffold = Scaffold
  { group :: Adw.PreferencesGroup
  , affirmButton :: Gtk.Button
  , isolateRow :: Adw.ActionRow
  , clearButton :: Gtk.Button
  , setRow :: Adw.SwitchRow
  , setRowSubtitle :: Text
  , closeDialog :: IO ()
  , presentDialog :: IO ()
  }

scaffold :: Adw.ApplicationWindow -> ScaffoldSpec -> IO Scaffold
scaffold window spec = do
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
      , #subtitle := spec.setSubtitle
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
  affirmButton <-
    new Gtk.Button [#label := spec.affirmLabel, #cssClasses := ["suggested-action"]]

  header <-
    new
      Adw.HeaderBar
      [ #showStartTitleButtons := False
      , #showEndTitleButtons := False
      ]
  header.packStart cancelButton
  header.packEnd affirmButton

  toolbarView <- new Adw.ToolbarView []
  headerWidget <- Gtk.toWidget header
  toolbarView.addTopBar headerWidget
  scrolledWidget <- Gtk.toWidget scrolled
  toolbarView.setContent (Just scrolledWidget)

  dialog <-
    new
      Adw.Dialog
      ( [ #title := spec.heading
        , #contentWidth := 460
        ]
          <> foldMap (\h -> [#contentHeight := h]) spec.contentHeight
      )
  toolbarWidget <- Gtk.toWidget toolbarView
  dialog.setChild (Just toolbarWidget)

  void $
    on pickButton #clicked $
      Dialog.pickFolder window "Isolate installation to…" spec.onIsolatePicked
  void $ on clearButton #clicked spec.onIsolateCleared
  void $ on cancelButton #clicked dialog.forceClose

  pure
    Scaffold
      { group
      , affirmButton
      , isolateRow
      , clearButton
      , setRow
      , setRowSubtitle = spec.setSubtitle
      , closeDialog = dialog.forceClose
      , presentDialog = dialog.present (Just window)
      }

renderIsolate :: Scaffold -> Maybe FilePath -> Bool -> IO ()
renderIsolate ui isolateDir setActive = do
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
    (if locked then "Isolated installs cannot be set as default" else ui.setRowSubtitle)
  ui.setRow.setActive setActive

wireSwitch :: Adw.SwitchRow -> IO Bool -> (Bool -> IO ()) -> IO ()
wireSwitch row getCurrent toggle =
  void $ on row (PropertyNotify #active) $ \_ -> do
    active <- row.getActive
    current <- getCurrent
    when (active /= current) (toggle active)

wireSetToggle :: Scaffold -> IO Bool -> (Bool -> IO ()) -> IO ()
wireSetToggle ui = wireSwitch ui.setRow

wireAffirm :: (Foldable f) => Scaffold -> IO (f a) -> (a -> IO ()) -> IO ()
wireAffirm ui getResult onAffirm =
  void $ on ui.affirmButton #clicked $ do
    result <- getResult
    forM_ result $ \a -> do
      ui.closeDialog
      onAffirm a

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
