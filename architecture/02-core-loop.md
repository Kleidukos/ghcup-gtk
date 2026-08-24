# The core loop

We use an Elm-style architecture: a single pure function receives the
current state and an event, and returns the new state plus a list of effects
to perform. The GTK code does not hold application state directly.

## The four pieces

All of this lives in `src/core/Session.hs`.

### Model

The model is a record of user preferences, tool versions, etc.
The `inFlight` field tracks which jobs are still running (by row key),
which is used to keep the application alive if the window closes mid-job.

The advanced table's sort column and filters are also just part of
`config`, so a header click or a filter checkbox is a `ConfigChanged`
like any other preference. See [Two renderers](06-two-renderers.md).

### Event

The known events of the application are in the `Event` data type.

### Effect

Using the Command Pattern, we build actions out of constructors of the
`Effect` type, so that we can observe in a pure fashion what would a user
interaction trigger before we execute our response to it.

Every operation in the application is a branch inside `step`. For
example: "when a mutation is submitted, hold the application open" is the
line that maps `Submitted job` to `[Hold, Enqueue job]`. Dimming the list
is not part of that branch: `step` is a thin wrapper around the branches
(internally `apply`) that compares `inFlight` before and after, and adds
`SetSensitive` itself on the edge between empty and non-empty — so the
list dims on the *first* mutation and re-enables on the *last* one to
finish, however many are running in between.

## How the UI loop runs

See `src/gtk/UI.hs`

`dispatch` is handed to every widget as its callback (a button click becomes
`dispatch (Session.Submitted …)`), and to the worker as its notifier (a
worker message becomes `dispatch (Session.WorkerMsg …)`).
The `interpretEffect` function is the place where effects are executed
(following the Command Pattern).


## Testability

`step` is a pure function over plain values. The test suite asserts things
like "a failed job re-renders the list and shows an error toast"
by comparing effect lists.

## Example: Installing GHC

1. The user clicks *Install* on a row. The row shows a confirmation dialog
   (`UI.View.List.Row` + `Presentation.Row.installConfirmation`); on confirm
   it calls `dispatch (Submitted (Install ghc version))`.
2. `step` sees a mutation: it adds the row's key to `inFlight` and returns
   `[Hold, Enqueue job]`; since this is the first mutation, `step` also adds
   `SetSensitive False`. The list dims; the job is queued.
3. The worker picks the job up and runs the real installation. While ghcup
   logs output, the worker emits `JobProgress` messages; `step` turns each
   into `SetBusy` effects, which the row renders as a progress bar.
4. The worker finishes and emits `JobDone job (Right ())`, then refreshes the
   listings and emits `ListingsReady`.
5. `step` handles `JobDone`: removes the row's key from `inFlight`, releases
   the application hold, sets the row idle, shows the "GHC X.Y.Z installed"
   toast. Then it handles `ListingsReady`: re-renders the lists and
   re-enables them.

On failure, step 5 instead restores the row's true state with the preceding
`SetIdle` (the accompanying re-render is a no-op: listings did not change,
so Registry's diff gate skips it) and emits an `ErrorToast` carrying the
real error text.
