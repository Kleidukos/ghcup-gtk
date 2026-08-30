module UI.View.ActionStrip
  ( build
  ) where

import Control.Monad (forM_, void, when)
import Data.GI.Base
import Data.Int (Int32)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Versions (Version)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Presentation.Row (RowAction (..), RowSpec (..), defaultAction, installMutation, installVerb, setDefaultMutation)
import Toolchain.Types (Progress (..), canCompileFromSource)
import UI.CompileOptionsDialog qualified as CompileOptionsDialog
import UI.InstallOptionsDialog qualified as InstallOptionsDialog
import UI.View (RowCallbacks (..), captionLabel)

windowOf :: (Gtk.IsWidget w) => w -> IO (Maybe Adw.ApplicationWindow)
windowOf widget =
  Gtk.widgetGetRoot widget >>= \case
    Nothing -> pure Nothing
    Just root -> castTo Adw.ApplicationWindow root

build
  :: RowCallbacks
  -> Gtk.CheckButton
  -> Int32
  -> [Version]
  -> RowSpec
  -> IO Gtk.Widget
build callbacks defaultGroup phaseWidth installedGhcs spec = do
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
      when (active && not spec.isDefault) $ callbacks.onSubmit (setDefaultMutation spec)
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
  let action = defaultAction spec
  actionButton <-
    new
      Gtk.Button
      [ #label := action.label
      , #valign := Gtk.AlignCenter
      , #visible := isNothing spec.progress
      ]
  void $ on actionButton #clicked $ callbacks.onConfirm action

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
  menuBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      ]
  optionsWidget <- Gtk.toWidget optionsItem
  menuBox.append optionsWidget
  popover <- new Gtk.Popover []
  menuBoxWidget <- Gtk.toWidget menuBox
  popover.setChild (Just menuBoxWidget)

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
    mwindow <- windowOf optionsItem
    forM_ mwindow $ \window ->
      InstallOptionsDialog.present window spec (callbacks.onSubmit . installMutation spec)

  when (canCompileFromSource spec.tool) $ do
    compileItem <-
      new
        Gtk.Button
        [ #label := "Compile from source…"
        , #cssClasses := ["flat"]
        ]
    compileWidget <- Gtk.toWidget compileItem
    menuBox.append compileWidget
    void $ on compileItem #clicked $ do
      popover.popdown
      mwindow <- windowOf compileItem
      forM_ mwindow $ \window ->
        CompileOptionsDialog.present window installedGhcs spec callbacks.onSubmit

  box.append menuButton

  forM_ spec.progress $ \progress -> do
    phaseLabel.setLabel progress.phase
    case progress.fraction of
      Just fraction -> progressBar.setFraction fraction
      Nothing -> progressBar.pulse

  Gtk.toWidget box
