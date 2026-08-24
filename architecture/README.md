# Architecture

These documents describe how ghcup-gtk is put together.

1. [Overview](01-overview.md): What the application is, the layers, the
   directory layout.
2. [The core loop](02-core-loop.md): How state, events, and effects flow
   through the application.
3. [Concurrency](03-concurrency.md): The GTK main thread, the worker
   thread, and how they talk to each other.
4. [Talking to ghcup](04-ghcup-integration.md): How the `Toolchain` layer
   wraps the `ghcup` library.
5. [PATH management](05-path-management.md): How the app detects and fixes
   a missing `PATH` entry for installed tools.
6. [Two renderers](06-two-renderers.md): The simple list and the advanced
   table behind one `View` seam.

## Overview

ghcup-gtk is a GTK4/libadwaita desktop frontend for [ghcup](https://www.haskell.org/ghcup/),
the Haskell toolchain manager. It shows the available versions of GHC, Cabal,
HLS, and Stack, and lets the user install, uninstall, and set defaults with a
click. All decisions live in a pure state machine (`Session.step`); all slow
ghcup operations run on a single background worker thread; the GTK code is a
thin shell that renders state and forwards clicks.

```
   user clicks                    GTK main thread
        │                               │
        ▼                               │
  ┌───────────┐   Event   ┌─────────────────────────┐
  │  UI (GTK) │ ────────► │  Session.step (pure)    │
  │  widgets  │ ◄──────── │  (Model, Event)         │
  └───────────┘  Effects  │    → (Model, [Effect])  │
        ▲                 └─────────────────────────┘
        │ WorkerMsg                     │ Enqueue effect
        │ (via GLib.idleAdd)            ▼
  ┌─────┴─────────────────────────────────────┐
  │  Worker thread (one job at a time)        │
  │  runs ghcup operations: list, install,    │
  │  uninstall, set default                   │
  └───────────────────────────────────────────┘
```
