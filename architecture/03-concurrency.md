# Concurrency

Exactly two threads, one rule for crossing between them.

## The two threads

### GTK main thread
GTK is not thread-safe: every widget call must happen on the thread running
the GTK main loop.

### Worker thread
ghcup operations are slow; on the main thread they would freeze the window.
`Worker` forks a single thread at startup that loops forever: take a `Job`
from an STM `TQueue`, run it, repeat. One thread because on-disk mutations
must be serialised.

## Crossing the boundary

Main → worker: push a `Job` onto the queue. Non-blocking, safe from
anywhere.

Worker → main: the worker never touches a widget. It reports through a
callback that schedules everything onto the GTK main loop via GLib's idle
mechanism. Only channel for results, progress, errors.

```
 main thread                      worker thread
 ───────────                      ─────────────
 event: Submitted job
   └─ effect: Enqueue job  ──►  TQueue ──► run job
                                      │ (minutes pass,
                                      │  progress lines)
 event: WorkerMsg …  ◄── GLib idle ──┘
```

## Worker operations

Each job runs in three steps:

### 1. Lazy environment setup
The ghcup environment (platform detection, directories, settings) is
created on the first job and memoized for the session; a failed acquisition
is not cached, so the next job retries. Setup failure becomes the job's own
failure: `JobDone mutation (Left err)` for a `Mutate` job, `ListingsFailed`
for `RefreshListings`.

### 2. Run the job
Jobs run via the `Ghcup` effect: the production interpreter delegates to
`Toolchain.GHCup`; tests interpret the same effect with pure handlers. Any
exception is caught and becomes `JobDone … (Left err)` rather than killing
the thread. Emission toward the UI goes through the `Notify` effect.

### 3. Relist after mutations
After a successful install/uninstall/set-default, the worker re-queries the
listings from the already-fetched metadata (full fetch as fallback), so the
UI shows the new reality without a redundant download.

## Progress reporting

ghcup writes log lines. The worker's log sink:

- remembers the current job (an `IORef`, written only by the worker thread);
- throttles to one message per 100 ms so a chatty download does not flood
  the main loop;
- forwards each surviving line as `JobProgress currentJob (Progress …)`.

Each line is recorded in the model and the affected tool's rows re-render:
pulsing progress bar plus latest log line. The 100 ms throttle is also the
redraw rate.

## Staying alive during a job

GTK applications quit when the last window closes; an interrupted GHC
install would leave a broken toolchain. A starting mutation emits `Hold`, a
finishing one `Release` (tracked by `inFlight`), mapping to
`Adw.Application`'s hold/release: the process stays alive until every
pending mutation is done.
