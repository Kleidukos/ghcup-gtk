# Two renderers

Each tool pane draws its version list one of two ways: simple
`Adw.ActionRow` list, or the "advanced interface" (a sortable, filterable
`Gtk.ColumnView` table). The `Config.advancedInterface` preference picks
which one exists: only the active renderer is ever built. Each pane holds
an `Adw.Bin`; switching modes replaces every pane's child with a freshly
built renderer and draws the plan carried by the `SwitchRenderer` effect.
No hidden renderer, nothing to go stale.

## The View

The `View` record in `UI.View` is the whole contract: a widget, row
replacement, sensitivity. `UI.Registry` is the only caller into a `View`;
`UI.View.List` and `UI.View.Table` each build one and never see each other.

## The registry

`UI.Registry` keeps one `View` per tool plus a cache of each tool's last
rows, so a rebuild only redraws tools whose rows changed. It also remembers
the last sensitivity value: Session emits sensitivity only on the change
edge, so freshly built renderers need it reapplied.

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

The table's sort column/direction and its two filters (`hlsPoweredOnly`,
`latestPatchOnly`) live in `config.kdl`. A header click or filter checkbox
dispatches `ConfigChanged`, which saves the config and fans the new state
to every tool's table via `SetTableState`.
Re-applying state there re-fires GTK's change signal, but the echoed update
matches the current config and the state machine drops it, so no loop. The
`SwitchRenderer` effect carries the persisted sort and filters, so a
freshly built table starts from them.
