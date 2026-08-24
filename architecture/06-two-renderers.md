# Two renderers

Each tool pane can draw its version list two ways: the simple
`Adw.ActionRow` list, or the "advanced interface" — a sortable, filterable
`Gtk.ColumnView` table. A preference (`Config.advancedInterface`) picks
which one is visible. Both are built at startup into the pane's inner
`Gtk.Stack` (`UI.ToolPanes.addView`), so switching is just changing which
stack page shows. Core has no idea which renderer is on screen; it only
ever hands over a plan.

## The View

The `View` record in `UI.View` is the whole contract: a widget, `setRows` to replace the
rendered rows, `setBusy`/`setIdle` per row key, and `setSensitive`.
`UI.Registry` is the only module that calls into a `View` — `UI.View.List`
(the simple renderer) and `UI.View.Table` (the advanced one) each build
one and never see each other.

## The registry

`UI.Registry` keeps one `View` per `(ViewMode, SupportedTool)`, a cache of
the rows last drawn to each renderer, and a map of rows currently in
progress. A rebuild only redraws the *visible* renderer, so a hidden
renderer's rows go stale on every refresh — which is why switching
renderers is a single `SwitchRenderer ViewMode plan` effect carrying the
fresh plan, never a bare flip: the newly-shown renderer is drawn with
current data in the same step, not left showing whatever it last saw.

## Busy state

- `Model.inFlight` — holds the application open and drives overall
  sensitivity while any mutation is running.
- The registry's progress map — replayed into a renderer's rows when that
  renderer is drawn or switched to.
- The table's own progress map — replayed onto a recycled cell when it is
  bound to a row (`Gtk.ColumnView` reuses cell widgets).

`setBusy` reaches only the visible views; `setIdle` reaches all of them,
so a job that finishes while its renderer is offscreen can't leave a
stuck row when the user switches back.

## The table's model chain

`UI.View.Table` holds row keys in a `Gtk.StringList`, wrapped in a
`Gtk.FilterListModel` then a `Gtk.SortListModel` feeding the
`Gtk.ColumnView`. haskell-gi cannot put a Haskell record inside a
GObject, so every sorter, filter, and cell callback looks the real
`RowSpec` up in a `Map Text RowSpec` by key. The version column sorts on
`RowSpec.rank` — core's newest-first index — so the table never parses a
version string itself.

## Persisted view state

The table's sort column/direction and its two filters
(`hlsPoweredOnly`, `latestPatchOnly`) live in `config.kdl`. A header
click or a filter checkbox dispatches `ConfigChanged`, which saves the
config and fans the new state to every tool's table via
`SetTableState` — including the table that originated the change.
Re-applying state to the originating table re-fires GTK's own change
signal, but the echoed update leaves the config unchanged, and
`echoesCurrentConfig` in `Session.step` stops it from looping.
