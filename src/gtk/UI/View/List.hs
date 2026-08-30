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

import Config (Config (..))
import Presentation.Filter (activeFilters, filtersFor)
import Presentation.Row (ToolRows (..), matchesFilters)
import UI.View (FilterBar (..), FiltersChanged, RowCallbacks, View (..), buildFilterBar, emptyStateStack)
import UI.View.List.Row qualified as Row

build
  :: Tool
  -> Config
  -> RowCallbacks
  -> FiltersChanged
  -> IO View
build tool config rowCallbacks onFiltersChanged = do
  let initial = activeFilters tool config.toolFilters
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

  bar <- buildFilterBar (filtersFor tool) initial (onFiltersChanged tool)

  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  content.append bar.widget
  content.append contentStack
  widget <- Gtk.toWidget content

  let setRows toolRows = do
        writeIORef rowsRef (Just toolRows)
        render

      setSensitive b = do
        set listBox [#sensitive := b]
        set bar.widget [#sensitive := b]

      applyConfig newConfig = do
        let filters = activeFilters tool newConfig.toolFilters
        writeIORef filtersRef filters
        bar.setFilters filters
        render

  pure View {widget, setRows, setSensitive, applyConfig}
