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
mutation maps to `[Hold, Enqueue job, Rerender …]`. Dimming the list is not
in that branch: a thin wrapper around the branches compares `inFlight`
before and after and adds `SetSensitive` on the empty↔non-empty edge.

## The UI loop

See `src/gtk/UI.hs`. One dispatch callback is handed to every widget (a
click becomes a `Submitted` event) and to the worker as its notifier (a
worker message becomes a `WorkerMsg` event). A single interpreter executes
the returned effects.

## Testability

The state machine is pure over plain values. Tests assert on effect lists:
"a failed job re-renders the list and shows an error toast".

## Example: Installing GHC

1. User clicks *Install* on a row; confirmation dialog; on confirm the row
   dispatches `Submitted (Install ghc version)`.
2. Mutation branch: row key added to `inFlight`, returns
   `[Hold, Enqueue job, Rerender …]`; first mutation also adds
   `SetSensitive False`. List dims, job queued, row shows spinner.
3. Worker runs the installation. ghcup log output becomes `JobProgress`
   messages; each is recorded in `inFlight` and re-rendered.
4. Worker emits `JobDone job (Right ())`, refreshes listings, emits
   `ListingsReady`.
5. `JobDone`: row key removed from `inFlight`, hold released, re-render
   (clears progress display), "GHC X.Y.Z installed" toast. `ListingsReady`:
   re-render with new listings, lists re-enabled.

On failure, the same re-render restores the row's true state and an
`ErrorToast` carries the real error text.
