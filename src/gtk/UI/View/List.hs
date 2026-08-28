module UI.View.List
  ( ListView (..)
  , ListCallbacks (..)
  , build
  ) where

import Control.Monad (forM, forM_)
import Data.GI.Base
import Data.IORef
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Config (Filters)
import Presentation.Row (ToolRows (..), matchesFilters)
import UI.View (FilterBar (..), View (..), buildFilterBar, emptyStateStack)
import UI.View.List.Row (RowHandle (..))
import UI.View.List.Row qualified as Row

-- | How the list reports filter changes
newtype ListCallbacks = ListCallbacks
  { onFiltersChanged :: Filters -> IO ()
  }

data ListView = ListView
  { view :: View
  , applyFilters :: Filters -> IO ()
  }

build
  :: Adw.ApplicationWindow
  -> Filters
  -> ListCallbacks
  -> IO ListView
build window initialFilters listCallbacks = do
  filtersRef <- newIORef initialFilters
  stateRef <- newIORef Nothing

  listBox <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  listBox.addCssClass "boxed-list"
  clamp <-
    new
      Adw.Clamp
      [ #child := listBox
      , #maximumSize := 600
      , #tighteningThreshold := 400
      , #marginTop := 24
      , #marginBottom := 24
      , #marginStart := 12
      , #marginEnd := 12
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
        readIORef stateRef >>= \case
          Nothing -> pure ()
          Just (callbacks, toolRows) -> do
            let visible = Vector.filter (matchesFilters filters) toolRows.rows
            handles <- forM visible $ \spec -> do
              handle <- Row.build window spec callbacks
              listBox.append handle.row
              pure handle
            -- One radio group per pane, anchored on the first installed row.
            case Vector.uncons (Vector.mapMaybe (.defaultCheck) handles) of
              Just (anchor, rest) -> forM_ rest $ \check -> check.setGroup (Just anchor)
              Nothing -> pure ()
            setEmpty (Vector.null visible)

  bar <- buildFilterBar initialFilters $ \filters -> do
    writeIORef filtersRef filters
    render
    listCallbacks.onFiltersChanged filters

  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  content.append bar.widget
  content.append contentStack
  widget <- Gtk.toWidget content

  let setRows callbacks toolRows = do
        writeIORef stateRef (Just (callbacks, toolRows))
        render

      setSensitive b = do
        set listBox [#sensitive := b]
        set bar.widget [#sensitive := b]

  pure ListView {view = View {widget, setRows, setSensitive}, applyFilters = bar.setFilters}
