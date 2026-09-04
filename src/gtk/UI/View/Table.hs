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
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Foreign.Ptr (Ptr, castPtr)
import GI.Adw qualified as Adw
import GI.GObject qualified as GObject
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Config (SortColumn (..), SortDirection (..), TableFilters (..), TableSort (..), sortColumnFromName, sortColumnName)
import Presentation.Row (RowAction (..), RowSpec (..), ToolRows (..))
import Toolchain.Types (Progress (..), rowKeyText)
import UI.Dialog qualified as Dialog
import UI.View (RowCallbacks (..), View (..), dimCaption, pillLabel)

-- | How the table reports state the user changed, for 'Config' to remember.
data TableCallbacks = TableCallbacks
  { onSortChanged :: TableSort -> IO ()
  , onFiltersChanged :: TableFilters -> IO ()
  }

-- | A built table: its 'View' plus the handle for applying state this table
-- did not originate.
data Table = Table
  { view :: View
  , applyState :: TableSort -> TableFilters -> IO ()
  }

-- | The widgets of one Actions cell. 'Gtk.ColumnView' recycles cells, so a
-- cell is registered on #bind and dropped on #unbind; 'setBusy' can then
-- find the row that is actually on screen.
data ActionCell = ActionCell
  { actionButton :: Gtk.Button
  , phaseLabel :: Gtk.Label
  , progressBar :: Gtk.ProgressBar
  , box :: Gtk.Box
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
  -> TableFilters
  -> TableCallbacks
  -> IO Table
build window initialSort initialFilters tableCallbacks = do
  specsRef <- newIORef Map.empty
  busyRef <- newIORef Map.empty
  cellsRef <- newIORef Map.empty
  filtersRef <- newIORef initialFilters
  callbacksRef <- newIORef (RowCallbacks (const (pure ())))

  items <- Gtk.stringListNew (Just [])

  rowFilter <- Gtk.customFilterNew . Just $ \obj -> do
    mspec <- specOfObject specsRef obj
    filters <- readIORef filtersRef
    pure (maybe True (matches filters) mspec)

  -- Property construction, never gtk_filter_list_model_new: the *_new
  -- constructors are transfer-full, haskell-gi disowns our wrapper, and a
  -- later filterChanged then touches a disowned pointer.
  filtered <- new Gtk.FilterListModel [#model := items, #filter := rowFilter]
  sorted <- new Gtk.SortListModel [#model := filtered]
  selection <- new Gtk.NoSelection [#model := sorted]

  columnView <- new Gtk.ColumnView [#showRowSeparators := True]
  columnView.setModel (Just selection)

  -- Version sorts on RowSpec.rank, which counts down from the newest row, so
  -- ascending on Down rank is ascending by version. No version parsing here.
  let textColumn :: (Ord a) => SortColumn -> Text -> (RowSpec -> Text) -> (RowSpec -> a) -> IO Gtk.ColumnViewColumn
      textColumn column title render sortKey =
        addColumn columnView specsRef (sortColumnName column) title plainCell (bindText render) noUnbind
          =<< fmap Just (mkSorter specsRef sortKey)
  versionColumn <-
    addColumn columnView specsRef (sortColumnName ByVersion) "Version" versionCell bindVersion noUnbind
      =<< fmap Just (mkSorter specsRef (\spec -> Down spec.rank))
  releasedColumn <- textColumn ByReleased "Released" dayText (.releaseDay)
  statusColumn <- textColumn ByStatus "Status" (.statusLabel) (\spec -> (spec.isDefault, spec.installed))
  void $
    addColumn
      columnView
      specsRef
      "actions"
      "Actions"
      actionsCell
      (bindActions window callbacksRef busyRef cellsRef)
      (unbindActions cellsRef)
      Nothing

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

  hlsCheck <-
    new Gtk.CheckButton [#label := "HLS-powered", #active := initialFilters.hlsPoweredOnly]
  latestCheck <-
    new
      Gtk.CheckButton
      [ #label := "Latest patch per major.minor"
      , #active := initialFilters.latestPatchOnly
      ]
  filterBar <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 12
      , #marginTop := 6
      , #marginBottom := 6
      , #marginStart := 12
      , #marginEnd := 12
      ]
  filterBar.append hlsCheck
  filterBar.append latestCheck

  emptyPage <-
    new
      Adw.StatusPage
      [ #title := "No versions match the filters"
      , #iconName := "system-search-symbolic"
      ]
  scrolled <- new Gtk.ScrolledWindow [#child := columnView, #vexpand := True]
  contentStack <- new Gtk.Stack []
  contentStack.addNamed scrolled (Just "rows")
  contentStack.addNamed emptyPage (Just "empty")

  let syncEmptyState = do
        count <- Gio.listModelGetNItems filtered
        contentStack.setVisibleChildName (if count == 0 then "empty" else "rows")

      syncFilters = do
        hls <- hlsCheck.getActive
        latest <- latestCheck.getActive
        let filters = TableFilters {hlsPoweredOnly = hls, latestPatchOnly = latest}
        writeIORef filtersRef filters
        Gtk.filterChanged rowFilter Gtk.FilterChangeDifferent
        syncEmptyState
        tableCallbacks.onFiltersChanged filters

  void $ on hlsCheck #toggled syncFilters
  void $ on latestCheck #toggled syncFilters
  void $ on filtered #itemsChanged $ \_position _removed _added -> syncEmptyState
  syncEmptyState

  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  content.append filterBar
  content.append contentStack
  widget <- Gtk.toWidget content

  let setRows callbacks toolRows = do
        writeIORef callbacksRef callbacks
        let keyed = [(rowKeyText spec.key, spec) | spec <- Vector.toList toolRows.rows]
        writeIORef specsRef (Map.fromList keyed)
        writeIORef cellsRef Map.empty
        previous <- Gio.listModelGetNItems items
        items.splice 0 previous (Just (map fst keyed))
        syncEmptyState

      setBusy key progress = do
        let keyText = rowKeyText key
        modifyIORef' busyRef (Map.insert keyText progress)
        cells <- readIORef cellsRef
        forM_ (Map.lookup keyText cells) $ \cell -> showBusy cell progress

      setIdle key = do
        let keyText = rowKeyText key
        modifyIORef' busyRef (Map.delete keyText)
        cells <- readIORef cellsRef
        forM_ (Map.lookup keyText cells) showIdle

      setSensitive b = do
        set columnView [#sensitive := b]
        set filterBar [#sensitive := b]

      -- Apply state this table did not originate. Setting the checkboxes fires
      -- #toggled, whose handler reports the value back; the model sees no
      -- change and emits nothing, so this does not loop.
      applyState sort filters = do
        applySort columnView columns sort
        hlsCheck.setActive filters.hlsPoweredOnly
        latestCheck.setActive filters.latestPatchOnly
        writeIORef filtersRef filters
        Gtk.filterChanged rowFilter Gtk.FilterChangeDifferent
        syncEmptyState

  pure Table {view = View {widget, setRows, setBusy, setIdle, setSensitive}, applyState}

matches :: TableFilters -> RowSpec -> Bool
matches filters spec =
  (not filters.hlsPoweredOnly || spec.passesHlsFilter)
    && (not filters.latestPatchOnly || spec.latestInFamily)

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
  -> (RowSpec -> Maybe Gtk.Widget -> IO ())
  -- ^ Run when a cell is recycled away from its row.
  -> Maybe Gtk.Sorter
  -> IO Gtk.ColumnViewColumn
addColumn columnView specsRef columnId title mkCell bindCell unbindCell sorter = do
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
  forM_ spec.pills $ \text -> do
    pill <- pillLabel text
    box.append pill
  Gtk.toWidget box

plainCell :: IO Gtk.Widget
plainCell = new Gtk.Label [#xalign := 0] >>= Gtk.toWidget

bindText :: (RowSpec -> Text) -> RowSpec -> Gtk.Widget -> IO ()
bindText render spec widget = do
  label <- unsafeCastTo Gtk.Label widget
  set label [#label := render spec]

dayText :: RowSpec -> Text
dayText spec = maybe "—" (Text.pack . show) spec.releaseDay

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
  Gtk.toWidget box

-- | Actions cells are rebuilt on every bind rather than mutated: a cell may
-- be recycled onto any row, so there is nothing stable to mutate.
bindActions
  :: Adw.ApplicationWindow
  -> IORef RowCallbacks
  -> IORef (Map Text Progress)
  -> IORef (Map Text ActionCell)
  -> RowSpec
  -> Gtk.Widget
  -> IO ()
bindActions window callbacksRef busyRef cellsRef spec widget = do
  box <- unsafeCastTo Gtk.Box widget
  drainChildren box
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
      , #visible := False
      , #maxWidthChars := 20
      , #ellipsize := Pango.EllipsizeModeEnd
      ]
  phaseLabel.addCssClass "caption"
  phaseLabel.addCssClass "dim-label"
  progressBar <- new Gtk.ProgressBar [#valign := Gtk.AlignCenter, #visible := False]
  actionButton <-
    new Gtk.Button [#label := spec.action.label, #valign := Gtk.AlignCenter]
  void $
    on actionButton #clicked $
      Dialog.confirm window spec.action.confirmation $ \confirmed ->
        when confirmed $ callbacks.onSubmit spec.action.job

  box.append phaseLabel
  box.append progressBar
  box.append actionButton

  let cell = ActionCell {actionButton, phaseLabel, progressBar, box}
      keyText = rowKeyText spec.key
  modifyIORef' cellsRef (Map.insert keyText cell)
  busy <- readIORef busyRef
  maybe (showIdle cell) (showBusy cell) (Map.lookup keyText busy)

showBusy :: ActionCell -> Progress -> IO ()
showBusy cell progress = do
  cell.actionButton.setVisible False
  cell.progressBar.setVisible True
  cell.phaseLabel.setVisible True
  cell.phaseLabel.setLabel progress.phase
  cell.progressBar.pulse

showIdle :: ActionCell -> IO ()
showIdle cell = do
  cell.progressBar.setVisible False
  cell.phaseLabel.setVisible False
  cell.actionButton.setVisible True

-- | Data cells hold no state worth dropping when they are recycled.
noUnbind :: RowSpec -> Maybe Gtk.Widget -> IO ()
noUnbind _ _ = pure ()

-- | Drop a recycled Actions cell, so 'setBusy' can never drive the widgets of
-- a row that has scrolled away. GTK may bind a row's key onto a new cell
-- before unbinding the old one, so the delete only takes effect when the
-- unbound child is still the one the cached cell was built from.
unbindActions :: IORef (Map Text ActionCell) -> RowSpec -> Maybe Gtk.Widget -> IO ()
unbindActions cellsRef spec mchild = do
  childPtr <- traverse (fmap castPtr . unsafeManagedPtrGetPtr) mchild
  cells <- readIORef cellsRef
  case (childPtr, Map.lookup (rowKeyText spec.key) cells) of
    (Just cp, Just cell) -> do
      cellPtr <- castPtr <$> unsafeManagedPtrGetPtr cell.box
      when (cellPtr == cp) $
        modifyIORef' cellsRef (Map.delete (rowKeyText spec.key))
    _ -> pure ()

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
