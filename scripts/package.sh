#!/usr/bin/env bash
set -euo pipefail

# Build distribution packages for ghcup-gtk with fpm.
#
# Usage: scripts/package.sh [format...]
#   formats: deb rpm pacman (Linux), osxpkg (macOS)
#   No arguments: build every format native to the host OS.
#
# Output lands in dist-package/out/.
#
# Make sure you keep the usage of the tools (tar, etc) compatible
# with both GNU and macOS.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OS="$(uname -s)"

if [ "$#" -gt 0 ]; then
  FORMATS=("$@")
elif [ "$OS" = "Darwin" ]; then
  FORMATS=(osxpkg)
else
  FORMATS=(deb rpm pacman)
fi

for fmt in "${FORMATS[@]}"; do
  case "$OS,$fmt" in
    Darwin,osxpkg | Linux,deb | Linux,rpm | Linux,pacman) ;;
    *)
      echo "error: cannot build '$fmt' on $OS" >&2
      exit 1
      ;;
  esac
done

if [ "$OS" = "Darwin" ]; then
  PREFIX=/usr/local
else
  PREFIX=/usr
fi

CABAL_VERSION="$(awk '/^version:/ {print $2}' ghcup-gtk.cabal)"
if TAG="$(git describe --tags --exact-match 2>/dev/null)"; then
  VERSION="${TAG#v}"
else
  VERSION="${CABAL_VERSION}.git$(git rev-parse --short HEAD)"
fi

echo "==> Building ghcup-gtk ${VERSION}"
CONFIGURE_FLAGS=(-f -development --datadir="${PREFIX}/share" --datasubdir=ghcup-gtk)
cabal build exe:ghcup-gtk "${CONFIGURE_FLAGS[@]}"
BIN="$(cabal list-bin ghcup-gtk "${CONFIGURE_FLAGS[@]}" | tail -1)"

echo "==> Staging filesystem root"
STAGING="dist-package/root"
rm -rf dist-package
mkdir -p dist-package/out

install_file() {
  local mode="$1" src="$2" dst="$3"
  mkdir -p "$(dirname "$dst")"
  install -m "$mode" "$src" "$dst"
}

install_file 755 "$BIN" "${STAGING}${PREFIX}/bin/ghcup-gtk"
strip "${STAGING}${PREFIX}/bin/ghcup-gtk"
install_file 644 data/style.css "${STAGING}${PREFIX}/share/ghcup-gtk/data/style.css"

if [ "$OS" = "Linux" ]; then
  install_file 644 data/org.haskell.GhcupGtk.desktop \
    "${STAGING}${PREFIX}/share/applications/org.haskell.GhcupGtk.desktop"
  install_file 644 data/org.haskell.GhcupGtk.svg \
    "${STAGING}${PREFIX}/share/icons/hicolor/scalable/apps/org.haskell.GhcupGtk.svg"
fi

FPM_COMMON=(
  -s dir
  -n ghcup-gtk
  -v "$VERSION"
  --license "GPL-3.0-only"
  --maintainer "Hécate Moonlight <hecate+github@glitchbra.in>"
  --description "GTK installer for the Haskell toolchain"
  --url "https://github.com/Kleidukos/ghcup-gtk"
  -a native
  -C "$STAGING"
  -p dist-package/out/
  -f
)

for fmt in "${FORMATS[@]}"; do
  EXTRA=()
  case "$fmt" in
    deb) EXTRA=(-d libgtk-4-1 -d libadwaita-1-0) ;;
    rpm) EXTRA=(-d gtk4 -d libadwaita) ;;
    pacman) EXTRA=(-d gtk4 -d libadwaita) ;;
    osxpkg) EXTRA=(--osxpkg-identifier-prefix org.haskell) ;;
  esac
  echo "==> fpm -t $fmt"
  fpm "${FPM_COMMON[@]}" -t "$fmt" "${EXTRA[@]}" .
done

echo "==> Packages:"
ls -l dist-package/out/
