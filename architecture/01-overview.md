# Overview

## What the application does

ghcup-gtk is a graphical installer for the Haskell toolchain.
It is a desktop frontend for the `ghcup` library. It:

- lists available and installed versions of tools (GHC, Cabal, HLS, and Stack, etc);
- installs and uninstalls versions, and sets the default version;
- warns the user when installed tools won't be found in their terminal (the
  ghcup bin directory is not on `PATH`) and offers to fix it;
- works offline by falling back to cached metadata.

## Deciding, Operating, Rendering

| Layer | Purpose |
|---|---|
| Decisions | Pure functions. No `IO` in the logic. This is where behavior lives, and it is what the test suite covers. |
| Operations | `IO` that touches the network, the filesystem, and the ghcup library. Slow and fallible. |
| Rendering | GTK widget construction and signal wiring. Deliberately dumb: it renders what it is told and forwards clicks as events. |

The point of the split is that everything interesting enough to get wrong is a pure function.
