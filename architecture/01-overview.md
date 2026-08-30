# Overview

## What the application does

Graphical frontend for the `ghcup` library:

- lists available and installed versions of GHC, Cabal, HLS, Stack, etc;
- installs, uninstalls, sets defaults;
- compiles GHC and HLS from source, with the same options as ghcup's TUI;
- warns when the ghcup bin directory is not on `PATH` and offers to fix it;
- works offline via cached metadata.

## Deciding, Operating, Rendering

| Layer | Purpose |
|---|---|
| Decisions | Pure functions, no `IO`. All behavior; all test coverage. |
| Operations | `IO`: network, filesystem, ghcup library. Slow and fallible. |
| Rendering | GTK widgets and signal wiring. Dumb: renders what it is told, forwards clicks as events. |

Everything interesting enough to get wrong is a pure function.

## Directory layout

- `src/core`: GTK-free library (`Session`, `Config`, `Presentation.*`,
  `Toolchain.*`, `Effects.*`, `Worker`). Test suite builds against this.
  Must never import a `gi-*` module.
- `src/gtk`: rendering layer, linked against `src/core`.
- `src/gtk/UI/View*`: the two renderers (`UI.View.List`, `UI.View.Table`)
  behind the single `View` record in `UI.View`.
- `src/gtk/UI/Shell.hs`: widget construction only, no model or callbacks.
- `src/gtk/UI.hs`: builds the shell, feeds events to the state
  machine, interprets the resulting effects.
