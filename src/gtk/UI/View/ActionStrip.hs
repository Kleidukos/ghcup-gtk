module UI.View.ActionStrip
  ( build
  ) where

import Control.Monad (forM_, void, when)
import Data.GI.Base
import Data.Int (Int32)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Presentation.Row (RowAction (..), RowSpec (..), installVerb)
import Toolchain.Types (Mutation (..), Progress (..))
import UI.Dialog qualified as Dialog
import UI.InstallOptionsDialog qualified as InstallOptionsDialog
import UI.View (RowCallbacks (..), captionLabel)

build
  :: Adw.ApplicationWindow
  -> RowCallbacks
  -> Gtk.CheckButton
  -> Int32
  -> RowSpec
  -> IO Gtk.Widget
build window callbacks defaultGroup phaseWidth spec = do
  box <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 6
      , #halign := Gtk.AlignEnd
      ]

  when spec.installed $ do
    check <-
      new
        Gtk.CheckButton
        [ #label := "Default"
        , #valign := Gtk.AlignCenter
        , #active := spec.isDefault
        , #sensitive := not spec.isDefault
        ]
    check.setGroup (Just defaultGroup)
    void $ on check #toggled $ do
      active <- check.getActive
      when (active && not spec.isDefault) $ callbacks.onSubmit spec.setDefault
    box.append check

  phaseLabel <-
    new
      Gtk.Label
      [ #valign := Gtk.AlignCenter
      , #visible := isJust spec.progress
      , #maxWidthChars := phaseWidth
      , #ellipsize := Pango.EllipsizeModeEnd
      ]
  captionLabel phaseLabel
  progressBar <-
    new Gtk.ProgressBar [#valign := Gtk.AlignCenter, #visible := isJust spec.progress]
  actionButton <-
    new
      Gtk.Button
      [ #label := spec.action.label
      , #valign := Gtk.AlignCenter
      , #visible := isNothing spec.progress
      ]
  void $
    on actionButton #clicked $
      Dialog.confirm window spec.action.confirmation $ \confirmed ->
        when confirmed $ callbacks.onSubmit spec.action.job

  box.append phaseLabel
  box.append progressBar
  box.append actionButton

  let menuLabel :: Text
      menuLabel = installVerb spec <> " with options…"
  optionsItem <-
    new
      Gtk.Button
      [ #label := menuLabel
      , #cssClasses := ["flat"]
      ]
  popover <- new Gtk.Popover []
  optionsWidget <- Gtk.toWidget optionsItem
  popover.setChild (Just optionsWidget)

  compileFromSourceItem <-
    new
      Gtk.Button
      [ #label := "Compile from source"
      , #cssClasses := ["flat"]
      ]
  compileFromSourceWidget <- Gtk.toWidget compileFromSourceItem
  -- popover.setChild (Just compileFromSourceWidget)

  menuButton <-
    new
      Gtk.MenuButton
      [ #iconName := "view-more-symbolic"
      , #valign := Gtk.AlignCenter
      , #cssClasses := ["flat"]
      , #visible := isNothing spec.progress
      ]
  menuButton.setPopover (Just popover)
  void $ on optionsItem #clicked $ do
    popover.popdown
    InstallOptionsDialog.present window spec $ \opts ->
      callbacks.onSubmit (Install spec.tool spec.installReq opts)
  -- void $ on compileFromSourceItem #clicked $ do

  box.append menuButton

  forM_ spec.progress $ \progress -> do
    phaseLabel.setLabel progress.phase
    case progress.fraction of
      Just fraction -> progressBar.setFraction fraction
      Nothing -> progressBar.pulse

  Gtk.toWidget box
