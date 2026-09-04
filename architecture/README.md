# Architecture

1. [Overview](01-overview.md): layers, directory layout.
2. [The core loop](02-core-loop.md): state, events, effects.
3. [Concurrency](03-concurrency.md): GTK main thread vs worker thread.
4. [Talking to ghcup](04-ghcup-integration.md): the `Toolchain` layer over the `ghcup` library.
5. [PATH management](05-path-management.md): detecting and fixing a missing `PATH` entry.

## Overview

ghcup-gtk is a GTK4/libadwaita frontend for [ghcup](https://www.haskell.org/ghcup/),
the Haskell toolchain manager: list, install, uninstall, compile from
source, and set default versions of GHC, Cabal, HLS, and Stack. All
decisions live in a pure state machine; all slow ghcup operations run on
one background worker thread; the GTK code renders state and forwards
clicks.

```
   user clicks                    GTK main thread
        │                               │
        ▼                               │
  ┌───────────┐   Event   ┌─────────────────────────┐
  │  UI (GTK) │ ────────► │  pure state machine     │
  │  widgets  │ ◄──────── │  (Model, Event)         │
  └───────────┘  Effects  │    → (Model, [Effect])  │
        ▲                 └─────────────────────────┘
        │ WorkerMsg                     │ Enqueue effect
        │ (via GLib idle)               ▼
  ┌─────┴─────────────────────────────────────┐
  │  Worker thread (one job at a time)        │
  │  runs ghcup operations: list, install,    │
  │  uninstall, set default                   │
  └───────────────────────────────────────────┘
```
