# Talking to ghcup

ghcup-gtk does not shell out to the `ghcup` binary. It links against the
`ghcup` **library** and calls the same internal commands the CLI uses. All of
that contact is confined to one module.

## Boundary

`src/core/Toolchain/GHCup.hs` is the only module that runs ghcup operations.

Two things happen at this boundary:

### Effect-system translation.
The ghcup library uses `Excepts` over `ReaderT AppState`.
Each wrapper runs that machinery and collapses whatever went wrong into a simple
`OpError { title, details }` (user-facing headline plus the error
for the "Details" dialog).

### State management.
`GhcupEnv` holds ghcup's `AppState` in an `IORef`, refreshed whenever
new metadata is fetched.

## Listings

"Listings" (`Toolchain.Types.Listings`) is a map from each tool to the
versions ghcup knows about: installed or not, tagged `recommended`/`latest`,
etc.

Producing listings has two phases with distinct failure behavior:

### 1. Fetch metadata

(`getDownloadsF`): downloads ghcup's release metadata.
If the network fetch fails, `fetchInfo` retries with ghcup's
`noNetwork = True` setting, which serves cached metadata from
`~/.ghcup/cache`. The result carries a `stale :: Bool` flag telling the UI
whether it is looking at cached data.

### 2. List versions
(`listVersions`): combines the metadata with what is actually installed on disk.

The UI maps the outcomes:

- fresh metadata → normal list;
- cached metadata → normal list plus the "version data may be outdated"
  banner (`stale = True`);
- no metadata at all (no network *and* no cache) → the "No Network
  Connection" page with a Retry button.

`relistListings` re-runs only phase 2 against the already-fetched metadata.
The worker uses it after every successful mutation so the UI updates without
a redundant download.

## Listing Curation

Raw listings include every version ghcup has ever published. Showing all of
them would be pretty useless, so `Toolchain.Curation.curate`
filters and sorts before rendering:

- drop versions with no binary distribution for this platform (unless
  already installed);
- keep only `recommended`, `latest`, and installed versions (unless the
  user enabled "show older versions" in the preferences);
- sort newest first.

Curation runs when `Session.step` plans the rows (`Presentation.Row.planRows`,
shipped to the UI in the `Rerender` effect, or in `SwitchRenderer` — the only
carrier of a plan on an interface toggle), not at fetch time: the Model
always holds the full listings, so toggling the preference re-renders instantly.

The advanced interface curates with `Full` instead: every installable
version is planned, the "show older versions" preference does not apply,
and narrowing the list is left to the table's own filters in the widget
layer.
