module UI.View.List
  ( build
  ) where

import Control.Monad (forM_)
import Data.GI.Base
import Data.IORef
import Data.Vector qualified as Vector
import GHCup.Types (Tool)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.Filter (defaultFilters, filtersFor)
import Presentation.Row (ToolRows (..), matchesFilters)
import UI.View (RowCallbacks, View (..), buildFilterBar, emptyStateStack)
import UI.View.List.Row qualified as Row

build
  :: Tool
  -> RowCallbacks
  -> IO View
build tool rowCallbacks = do
  let initial = defaultFilters tool
  filtersRef <- newIORef initial
  rowsRef <- newIORef Nothing

  defaultGroup <- new Gtk.CheckButton []

  listBox <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  listBox.addCssClass "boxed-list"
  clamp <-
    new
      Adw.Clamp
      [ #child := listBox
      , #maximumSize := 600
      , #tighteningThreshold := 400
      , #cssClasses := ["list-container"]
      ]
  scrolled <-
    new
      Gtk.ScrolledWindow
      [ #child := clamp
      , #vexpand := True
      , #hscrollbarPolicy := Gtk.PolicyTypeNever
      ]
  scrolledWidget <- Gtk.toWidget scrolled
  (contentStack, setEmpty) <- emptyStateStack scrolledWidget

  let render = do
        filters <- readIORef filtersRef
        listBox.removeAll
        readIORef rowsRef >>= \case
          Nothing -> pure ()
          Just toolRows -> do
            let visible = Vector.filter (matchesFilters filters) toolRows.rows
            forM_ visible $ \spec -> do
              row <- Row.build defaultGroup toolRows.installedGhcs spec rowCallbacks
              listBox.append row
            setEmpty (Vector.null visible)

  let onFiltersChanged filters = do
        writeIORef filtersRef filters
        render
  bar <- buildFilterBar (filtersFor tool) initial onFiltersChanged

  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  content.append bar
  content.append contentStack
  widget <- Gtk.toWidget content

  let setRows toolRows = do
        writeIORef rowsRef (Just toolRows)
        render

      setSensitive b = do
        set listBox [#sensitive := b]
        set bar [#sensitive := b]

      applyConfig _ = pure ()

  pure View {widget, setRows, setSensitive, applyConfig}
