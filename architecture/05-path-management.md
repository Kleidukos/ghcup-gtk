# PATH management

Logic in `src/core/Toolchain/Path.hs`, user-facing text in
`Presentation.Path`, widget in `UI.PathBanner`.

Runs through the core loop: `PathChecked`, `PathFixConfirmed`, and
`PathFixDone` events drive the state machine, which tracks a `PathModel`
and emits `CheckPath`, `ApplyPathFix`, and `SetPathBanner` effects.

## Detection

At startup, and again after each successful mutation. The check classifies
into a `PathStatus`:

| Status | Situation | Banner |
|---|---|---|
| `PathOk` | ghcup bin directory on `PATH` | none |
| `FixedAwaitingRestart` | not on `PATH`, but a shell config file already has the marker line (fix applied) | "Restart your terminal" |
| `NeedsFixPlanned` | not on `PATH`, no marker, shell recognized (bash, zsh, fish) | **Fix…** button |
| `NeedsFixManual` | same, shell unknown. We don't guess which file to edit | manual instructions: exact line to add |
