# Talking to ghcup

No shelling out to the `ghcup` binary: ghcup-gtk links against the `ghcup`
library and calls the same internal commands the CLI uses. All contact
confined to one module.

## Boundary

`src/core/Toolchain/GHCup.hs` is the only module that runs ghcup
operations. Two jobs at this boundary:

### Effect-system translation
The ghcup library uses `Excepts` over `ReaderT AppState`. Each wrapper runs
that machinery and collapses failures into `OpError { title, details }`
(user-facing headline plus error for the "Details" dialog).

### State management
`GhcupEnv` holds ghcup's `AppState` in an `IORef`, refreshed whenever new
metadata is fetched.

## Listings

`Toolchain.Types.Listings`: a map from each tool to the versions ghcup
knows, tagged `recommended`/`latest`, etc.

Two phases, distinct failure behavior:

### 1. Fetch metadata
Downloads ghcup's release metadata. On network failure, retries with
ghcup's `noNetwork = True`, which serves cached metadata from
`~/.ghcup/cache`. The result carries `stale :: Bool` telling the UI whether
it is looking at cached data.

### 2. List versions
Combines the metadata with what is installed on disk.

UI mapping:

- fresh metadata → normal list;
- cached metadata (`stale = True`) → normal list plus "version data may be
  outdated" banner;
- no metadata at all (no network, no cache) → "No Network Connection" page
  with Retry.

A relist operation re-runs only phase 2 against the already-fetched
metadata; the worker uses it after every successful mutation.

## Listing curation

Raw listings include every version ghcup ever published. Curation filters
and sorts before rendering (`Toolchain.Curation`):

- drop versions with no binary distribution for this platform (unless
  installed);
- keep only `recommended`, `latest`, and installed (unless "show older
  versions" is enabled in preferences);
- sort newest first.

Curation runs when the state machine plans the rows (`Presentation.Row`,
shipped in the `Rerender` effect, or in `SwitchRenderer`.

The advanced interface curates with `Full`: every installable version is
planned, "show older versions" does not apply, narrowing is left to the
table's own filters in the widget layer.
