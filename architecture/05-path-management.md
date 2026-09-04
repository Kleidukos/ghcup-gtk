# PATH management

All logic lives in `src/core/Toolchain/Path.hs`, user-facing text
in `Presentation.Path`, and the widget in `UI.PathBanner`.

The flow itself runs through the core loop: `PathChecked`,
`PathFixConfirmed`, and `PathFixDone` events drive `Session.step`, which
tracks a `PathModel` in the model and emits `CheckPath`, `ApplyPathFix`, and
`SetPathBanner` effects.

## Detection (at startup, and again after each successful mutation)

`checkPath` classifies the situation into a `PathStatus`:

### `PathOk`
The ghcup bin directory is already on `PATH`. No banner.


### `FixedAwaitingRestart`
`PATH` doesn't have it, but a shell config file already contains the marker line.
The fix has been applied and the current session predates it.
Banner is "Restart your terminal".

### `NeedsFix (Just plan)`
Not on `PATH`, no marker, and we recognize the user's shell (bash, zsh, or fish).
Banner with a **Fix…** button.

### `NeedsFix Nothing`
Same, but the shell is unknown. We won't guess which file to edit, so the banner shows manual instructions instead: the exact line to add, for the user to place themselves.

