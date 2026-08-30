# Two renderers

Each tool pane draws its version list one of two ways: simple
`Adw.ActionRow` list, or the "advanced interface" (a sortable, filterable
`Gtk.ColumnView` table). The `Config.viewMode` preference picks which one
exists: only the active renderer is ever built. Each pane holds an
`Adw.Bin`; when the registry sees a different view mode in an incoming
reconcile it replaces every pane's child with a freshly built renderer and
replays the current state onto it. No hidden renderer, nothing to go
stale.

## The View

The `View` record in `UI.View` is the whole contract: a widget, row
replacement (`setRows`), sensitivity, and `applyConfig` for persisted view
state. `UI.Registry` is the only caller into a `View`; `UI.View.List` and
`UI.View.Table` each build one and never see each other.

## The registry

`UI.Registry` keeps one `View` per tool plus the last `ViewState` it
applied, and diffs each incoming state against it: a pane-set or view-mode
change rebuilds the renderers, sensitivity is reapplied on its change
edge, a config change fans out through `applyConfig`, and `setRows` runs
only for tools whose rows changed. After a rebuild the whole state is
replayed onto the fresh widgets.

## Busy state

A running mutation lives in `Model.inFlight` (`Map RowKey Progress`),
which both holds the application open and stamps progress into the
`RowSpec` when the plan is computed. Renderers draw a stamped row as a
pulsing bar plus latest log line.

## The table's model chain

`UI.View.Table` holds row keys in a `Gtk.StringList`, wrapped in a
`Gtk.FilterListModel` then a `Gtk.SortListModel` feeding the
`Gtk.ColumnView`. haskell-gi cannot put a Haskell record inside a GObject,
so every sorter, filter, and cell callback looks the real `RowSpec` up in a
`Map Text RowSpec` by key. The version column sorts on the spec's rank
so the table never parses a version string.
Cells are rebuilt on every bind rather than recycled: a cell may land on
any row, and the table is small enough that widget churn does not matter.
The rebuild reads current progress from the spec, so busy rows need no
special handling.

## Persisted view state

The table's sort column/direction and both filter bars (`hlsPoweredOnly`,
`latestPatchOnly`, per renderer) live in `config.kdl`. A header click or
filter checkbox only dispatches `ConfigChanged`; the state machine saves
the config and reconciles, and `applyConfig` applies it back to every pane
and re-renders. Data flows one way: widget → event → model → reconcile → widget.
Re-applying state re-fires GTK's change signal, but the echoed update
matches the current config and the state machine drops it, so no loop. A
freshly built renderer starts from the persisted config it was built
with.
