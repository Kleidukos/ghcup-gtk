# The core loop

Elm-style architecture: one pure function takes `(Model, Event)` and
returns `(Model, [Effect])`. GTK code holds no application state.

## The pieces

All in `src/core/Session.hs`.

### Model

Record of user preferences, tool versions, etc. The `inFlight` field maps
row keys to running jobs and their latest progress line; it keeps the
application alive if the window closes mid-job and feeds each row's
progress display.

### Event

The `Event` data type enumerates everything that can happen.

### Effect

Command pattern: the `Effect` type describes actions as data, so tests can
observe what an interaction would trigger without executing it.

Each operation is one branch of the state machine. Example: a submitted
mutation maps to `[Hold, Enqueue job, Reconcile]`. Rendering is one coarse
effect: `Reconcile` tells the UI to re-read the model and re-derive
everything it shows (row plan, visible page, banners, sensitivity).
`UI.Registry` diffs that view state against the last one it applied, so
the coarse effect still repaints only what changed.

Dialog policy is also data: a row action that needs confirmation arrives
as `ConfirmRequested` and maps to a `Confirm confirmation job` effect; the
interpreter shows the dialog and dispatches `Submitted job` on accept.

## The UI loop

See `src/gtk/UI.hs`. One dispatch callback is handed to every widget (a
click becomes a `Submitted` or `ConfirmRequested` event) and to the worker
as its notifier (a worker message becomes a `WorkerMsg` event). A single
interpreter executes the returned effects.

## Testability

The state machine is pure over plain values. Tests assert on effect lists:
"a failed job re-renders the list and shows an error toast".

## Example: Installing GHC

1. User clicks *Install* on a row; the row dispatches `ConfirmRequested`
   with the action's confirmation text and job; the `Confirm` effect shows
   the dialog; on confirm the interpreter dispatches `Submitted (Mutate
   (Install ghc version …))`.
2. Mutation branch: row key added to `inFlight`, returns
   `[Hold, Enqueue job, Reconcile]`. The reconcile derives sensitivity
   from `inFlight`, so the list dims; job queued, row shows progress.
3. Worker runs the installation. ghcup log output becomes `JobProgress`
   messages; each is recorded in `inFlight` and reconciled.
4. Worker emits `JobDone job (Right ())`, re-lists from the cached
   metadata, emits `Relisted`.
5. `JobDone`: row key removed from `inFlight`, hold released, reconcile
   (clears progress display), "GHC X.Y.Z installed" toast. `Relisted`:
   reconcile with new listings, lists re-enabled.

On failure, the same re-render restores the row's true state and an
`ErrorToast` carries the real error text.
