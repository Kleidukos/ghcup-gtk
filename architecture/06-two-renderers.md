# Two renderers

Each tool pane can draw its version list two ways: the simple
`Adw.ActionRow` list, or the "advanced interface" — a sortable, filterable
`Gtk.ColumnView` table. A preference (`Config.advancedInterface`) picks
which one exists: only the active renderer is ever built. Each pane holds
an `Adw.Bin`; switching modes replaces every pane's child with a freshly
built renderer (`UI.Registry.switchTo`) and draws the plan the
`SwitchRenderer` effect carries. There is no hidden renderer, so nothing
can go stale.

## The View

The `View` record in `UI.View` is the whole contract: a widget, `setRows`
to replace the rendered rows, and `setSensitive`. `UI.Registry` is the
only module that calls into a `View` — `UI.View.List` (the simple
renderer) and `UI.View.Table` (the advanced one) each build one and never
see each other.

## The registry

`UI.Registry` keeps one `View` per tool and a cache of the rows each tool
last drew, so a rebuild only redraws tools whose rows changed. It also
remembers the last sensitivity value: Session emits sensitivity only on
the change edge, so freshly built renderers must have it reapplied.

## Busy state

A running mutation lives in `Model.inFlight` (`Map RowKey Progress`),
which both holds the application open and stamps `RowSpec.progress` when
the plan is computed. Renderers draw a stamped row as a pulsing bar plus
the latest log line; there is no other busy bookkeeping. A renderer
switch mid-mutation shows its spinners immediately, because they are in
the plan it is drawn from.

## The table's model chain

`UI.View.Table` holds row keys in a `Gtk.StringList`, wrapped in a
`Gtk.FilterListModel` then a `Gtk.SortListModel` feeding the
`Gtk.ColumnView`. haskell-gi cannot put a Haskell record inside a
GObject, so every sorter, filter, and cell callback looks the real
`RowSpec` up in a `Map Text RowSpec` by key. The version column sorts on
`RowSpec.rank` — core's newest-first index — so the table never parses a
version string itself. Cell recycling needs no special handling for busy
rows: a recycled cell is always rebound, and the rebind reads current
progress from the spec it looks up.

## Persisted view state

The table's sort column/direction and its two filters
(`hlsPoweredOnly`, `latestPatchOnly`) live in `config.kdl`. A header
click or a filter checkbox dispatches `ConfigChanged`, which saves the
config and fans the new state to every tool's table via
`SetTableState` — including the table that originated the change.
Re-applying state to the originating table re-fires GTK's own change
signal, but the echoed update leaves the config unchanged, and
`echoesCurrentConfig` in `Session.step` stops it from looping. The
`SwitchRenderer` effect carries the persisted sort and filters too, so a
freshly built table starts from them.
