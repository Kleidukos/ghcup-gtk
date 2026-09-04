# Concurrency

There are exactly two threads, and one rule about crossing between them.

## The two threads

### The GTK main thread
GTK is not thread-safe: every widget call must happen on the thread
running the GTK main loop.
All of `UI.*`, and therefore all of `dispatch`/`interpretEffect`/`step`, run here.

### The worker thread
ghcup operations are slow and must never run on the main thread,
or the window would freeze.
The `Worker` forks a single thread at startup that loops forever:
take a `Job` from an STM `TQueue`, run it, rinse, repeat.

We do this because mutating the on-disk state is unsafe, so operations
need to be serialised.

## Crossing the boundary between Good and Evil

**Main thread → worker:** `Worker.enqueue` pushes a `Job` onto the queue.
This is non-blocking and safe from anywhere.

**Worker → main thread:** the worker never touches a widget. It reports
through a callback that wraps everything in `GLib.idleAdd`.
`idleAdd` schedules the action to run on the GTK main loop.
This is the only mechanism the worker uses to communicate results,
progress, and errors.

```
 main thread                      worker thread
 ───────────                      ─────────────
 dispatch(Submitted job)
   └─ interpretEffect(Enqueue job) ──►  TQueue ──► runJob
                                              │ (minutes pass,
                                              │  progress lines)
 dispatch(WorkerMsg …)  ◄── GLib.idleAdd ─────┘
   └─ interpretEffect(WorkerMsg)
```

## Worker operations

`Worker.processJob` handles each job in three steps:

### 1. Lazy environment setup.
The ghcup environment (platform detection, directories, settings) is created
on the first job by `GHCup.newEnv` and memoized by `withEnv`, a helper local
to `runGhcupIO`, for the rest of the session; a failed acquisition is not
cached, so the next job retries it. If setup fails (e.g. unsupported
platform), it becomes the job's own failure result: `JobDone mutation (Left
err)` for a `Mutate` job, `ListingsFailed` only for `RefreshListings`.

### 2. Run the job
Jobs are run via the `Ghcup` effect. `Worker.start` runs the loop under the
interpreter `runGhcupIO`, which delegates to the real `Toolchain.GHCup`
functions; tests interpret the same effect with pure handlers. Any exception
is caught and turned into a `JobDone … (Left err)` message rather than
killing the thread. Emission toward the UI goes through the `Notify` effect.

### 3. Relist after mutations
After a successful install/uninstall/set-default, the worker immediately
re-queries the listings so the UI shows the new reality without a second network
fetch (it relists from the already-fetched metadata, falling back to a full
fetch if that fails).

## Progress reporting

ghcup writes log lines. The worker installs a log sink that:

- remembers which job is currently running (an `IORef`, written only by the
  worker thread);
- throttles to at most one message every 100 ms, so a chatty download does
  not flood the main loop;
- forwards each surviving line as `JobProgress currentJob (Progress …)`.

The UI records each surviving line in the model and re-renders the
affected tool's rows, which draw a pulsing progress bar plus the latest
log line. The 100 ms throttle is therefore also the redraw rate.

## Staying alive during a job

GTK applications normally quit when the last window closes. An interrupted
GHC install would leave a broken toolchain, so `step` emits `Hold` when a
mutation starts and `Release` when it finishes (tracked by
`inFlight`). `Hold`/`Release` map to `Adw.Application`'s hold/release
mechanism: the process stays alive until every pending mutation is done.
