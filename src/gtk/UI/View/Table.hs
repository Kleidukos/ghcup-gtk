module UI.View.Table
  ( Table (..)
  , TableCallbacks (..)
  , build
  ) where

import Control.Monad (forM_, void, when)
import Data.GI.Base
import Data.IORef
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Vector qualified as Vector
import Foreign.Ptr (Ptr, castPtr)
import GI.Adw qualified as Adw
import GI.GObject qualified as GObject
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Config (Filters, SortColumn (..), SortDirection (..), TableSort (..), sortColumnFromName, sortColumnName)
import Presentation.Row (RowAction (..), RowSpec (..), ToolRows (..), matchesFilters)
import Toolchain.Types (Progress (..), rowKeyText)
import UI.Dialog qualified as Dialog
import UI.View (FilterBar (..), RowCallbacks (..), View (..), buildFilterBar, captionLabel, emptyStateStack, pillLabel)

-- | How the table reports state the user changed, for 'Config' to remember.
data TableCallbacks = TableCallbacks
  { onSortChanged :: TableSort -> IO ()
  , onFiltersChanged :: Filters -> IO ()
  }

-- | A built table: its 'View' plus the handle for applying state this table
-- did not originate.
data Table = Table
  { view :: View
  , applyState :: TableSort -> Filters -> IO ()
  }

-- | The advanced renderer: a sortable, filterable 'Gtk.ColumnView'.
--
-- Rows live in a 'Gtk.StringList' of 'rowKeyText' keys, with the real
-- 'RowSpec' in a Haskell map beside it: haskell-gi cannot define a GObject
-- subclass carrying a Haskell record, so every sorter, filter and cell
-- callback looks its row up by key.
build
  :: Adw.ApplicationWindow
  -> TableSort
  -> Filters
  -> TableCallbacks
  -> IO Table
build window initialSort initialFilters tableCallbacks = do
  specsRef <- newIORef Map.empty
  filtersRef <- newIORef initialFilters
  callbacksRef <- newIORef (RowCallbacks (const (pure ())))

  items <- Gtk.stringListNew (Just [])

  rowFilter <- Gtk.customFilterNew . Just $ \obj -> do
    mspec <- specOfObject specsRef obj
    filters <- readIORef filtersRef
    pure (maybe True (matchesFilters filters) mspec)

  -- Property construction, never gtk_filter_list_model_new: the *_new
  -- constructors are transfer-full, haskell-gi disowns our wrapper, and a
  -- later filterChanged then touches a disowned pointer.
  filtered <- new Gtk.FilterListModel [#model := items, #filter := rowFilter]
  sorted <- new Gtk.SortListModel [#model := filtered]
  selection <- new Gtk.NoSelection [#model := sorted]

  columnView <- new Gtk.ColumnView [#showRowSeparators := True, #cssClasses := ["zebra-stripes"]]
  columnView.setModel (Just selection)

  -- Version sorts on RowSpec.rank, which counts down from the newest row, so
  -- ascending on Down rank is ascending by version. No version parsing here.
  let textColumn :: (Ord a) => SortColumn -> Text -> (RowSpec -> Text) -> (RowSpec -> a) -> IO Gtk.ColumnViewColumn
      textColumn column title render sortKey = do
        sorter <- mkSorter specsRef sortKey
        addColumn columnView specsRef (sortColumnName column) title (textCell render) (Just sorter)
  versionSorter <- mkSorter specsRef (\spec -> Down spec.rank)
  versionColumn <-
    addColumn columnView specsRef (sortColumnName ByVersion) "Version" versionCell (Just versionSorter)
  releasedColumn <- textColumn ByReleased "Released" dayText (.releaseDay)
  statusColumn <- textColumn ByStatus "Status" (.statusLabel) (\spec -> (spec.isDefault, spec.installed))
  void $
    addColumn columnView specsRef "actions" "Actions" (actionsCell window callbacksRef) Nothing

  let columns =
        [ (ByVersion, versionColumn)
        , (ByReleased, releasedColumn)
        , (ByStatus, statusColumn)
        ]

  viewSorter <- columnView.getSorter
  sorted.setSorter viewSorter
  applySort columnView columns initialSort

  forM_ viewSorter $ \sorter ->
    void $ on sorter #changed $ \_change -> do
      columnSorter <- unsafeCastTo Gtk.ColumnViewSorter sorter
      mcolumn <- columnSorter.getPrimarySortColumn
      order <- columnSorter.getPrimarySortOrder
      forM_ mcolumn $ \column -> do
        mid <- column.getId
        forM_ (mid >>= sortColumnFromName) $ \sortColumn ->
          tableCallbacks.onSortChanged (TableSort sortColumn (directionOf order))

  scrolled <- new Gtk.ScrolledWindow [#child := columnView, #vexpand := True]
  scrolledWidget <- Gtk.toWidget scrolled
  (contentStack, setEmpty) <- emptyStateStack scrolledWidget

  let syncEmptyState = do
        count <- Gio.listModelGetNItems filtered
        setEmpty (count == 0)

  bar <- buildFilterBar initialFilters $ \filters -> do
    writeIORef filtersRef filters
    Gtk.filterChanged rowFilter Gtk.FilterChangeDifferent
    syncEmptyState
    tableCallbacks.onFiltersChanged filters

  void $ on filtered #itemsChanged $ \_position _removed _added -> syncEmptyState
  syncEmptyState

  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  content.append bar.widget
  content.append contentStack
  widget <- Gtk.toWidget content

  let setRows callbacks toolRows = do
        writeIORef callbacksRef callbacks
        let keyed = [(rowKeyText spec.key, spec) | spec <- Vector.toList toolRows.rows]
        writeIORef specsRef (Map.fromList keyed)
        previous <- Gio.listModelGetNItems items
        items.splice 0 previous (Just (map fst keyed))
        syncEmptyState

      setSensitive b = do
        set columnView [#sensitive := b]
        set bar.widget [#sensitive := b]

      applyState sort filters = do
        applySort columnView columns sort
        bar.setFilters filters

  pure Table {view = View {widget, setRows, setSensitive}, applyState}

-- | Row behind a 'Gtk.StringObject' handed to a signal callback.
specOfObject :: IORef (Map Text RowSpec) -> GObject.Object -> IO (Maybe RowSpec)
specOfObject specsRef obj = do
  stringObject <- unsafeCastTo Gtk.StringObject obj
  key <- stringObject.getString
  Map.lookup key <$> readIORef specsRef

-- | Row behind a borrowed pointer handed to a GCompareDataFunc. The
-- annotation on 'castPtr' is mandatory: without it the GObject dictionary is
-- ambiguous and GHC 9.14.1 panics with lookupIdSubst.
specOfPtr :: IORef (Map Text RowSpec) -> Ptr () -> IO (Maybe RowSpec)
specOfPtr specsRef p = do
  stringObject <- newObject Gtk.StringObject (castPtr p :: Ptr Gtk.StringObject)
  key <- stringObject.getString
  Map.lookup key <$> readIORef specsRef

mkSorter :: (Ord a) => IORef (Map Text RowSpec) -> (RowSpec -> a) -> IO Gtk.Sorter
mkSorter specsRef project = do
  sorter <- Gtk.customSorterNew (Just compareRows)
  Gtk.toSorter sorter
  where
    compareRows :: Ptr () -> Ptr () -> IO Int32
    compareRows a b = do
      left <- specOfPtr specsRef a
      right <- specOfPtr specsRef b
      pure $ case (left, right) of
        (Just l, Just r) -> case compare (project l) (project r) of
          LT -> -1
          EQ -> 0
          GT -> 1
        _ -> 0

addColumn
  :: Gtk.ColumnView
  -> IORef (Map Text RowSpec)
  -> Text
  -- ^ Column id: how a persisted sort column is matched back to a column.
  -> Text
  -- ^ Title.
  -> (RowSpec -> IO Gtk.Widget)
  -> Maybe Gtk.Sorter
  -> IO Gtk.ColumnViewColumn
addColumn columnView specsRef columnId title makeCell sorter = do
  factory <- new Gtk.SignalListItemFactory []
  -- Cells are rebuilt on every bind rather than recycled: a cell may land on
  -- any row, and the table is small enough that widget churn does not matter.
  void $ on factory #bind $ \obj -> do
    item <- unsafeCastTo Gtk.ListItem obj
    mitem <- item.getItem
    forM_ mitem $ \gobj -> do
      mspec <- specOfObject specsRef gobj
      forM_ mspec $ \spec -> do
        cell <- makeCell spec
        item.setChild (Just cell)
  column <-
    new
      Gtk.ColumnViewColumn
      [ #title := title
      , #factory := factory
      , #expand := True
      , #id := columnId
      ]
  forM_ sorter $ \s -> column.setSorter (Just s)
  columnView.appendColumn column
  pure column

versionCell :: RowSpec -> IO Gtk.Widget
versionCell spec = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  label <- new Gtk.Label [#label := spec.title, #xalign := 0]
  box.append label
  forM_ spec.pills $ \p -> do
    pill <- pillLabel (display p)
    box.append pill
  Gtk.toWidget box

textCell :: (RowSpec -> Text) -> RowSpec -> IO Gtk.Widget
textCell render spec = new Gtk.Label [#label := render spec, #xalign := 0] >>= Gtk.toWidget

dayText :: RowSpec -> Text
dayText spec = maybe "–" (Text.pack . show) spec.releaseDay

actionsCell
  :: Adw.ApplicationWindow
  -> IORef RowCallbacks
  -> RowSpec
  -> IO Gtk.Widget
actionsCell window callbacksRef spec = do
  box <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 6
      , #halign := Gtk.AlignEnd
      ]
  callbacks <- readIORef callbacksRef

  when spec.installed $ do
    check <-
      new
        Gtk.CheckButton
        [ #label := "Default"
        , #valign := Gtk.AlignCenter
        , #active := spec.isDefault
        , #sensitive := not spec.isDefault
        ]
    void $ on check #toggled $ do
      active <- check.getActive
      when (active && not spec.isDefault) $ callbacks.onSubmit spec.setDefault
    box.append check

  phaseLabel <-
    new
      Gtk.Label
      [ #valign := Gtk.AlignCenter
      , #visible := isJust spec.progress
      , #maxWidthChars := 20
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

  forM_ spec.progress $ \progress -> do
    phaseLabel.setLabel progress.phase
    case progress.fraction of
      Just fraction -> progressBar.setFraction fraction
      Nothing -> progressBar.pulse

  Gtk.toWidget box

applySort :: Gtk.ColumnView -> [(SortColumn, Gtk.ColumnViewColumn)] -> TableSort -> IO ()
applySort columnView columns tableSort =
  forM_ (lookup tableSort.column columns) $ \column ->
    columnView.sortByColumn (Just column) (sortTypeOf tableSort.direction)

sortTypeOf :: SortDirection -> Gtk.SortType
sortTypeOf = \case
  Ascending -> Gtk.SortTypeAscending
  Descending -> Gtk.SortTypeDescending

directionOf :: Gtk.SortType -> SortDirection
directionOf = \case
  Gtk.SortTypeDescending -> Descending
  _ -> Ascending
