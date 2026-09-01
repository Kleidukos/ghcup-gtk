#!/usr/bin/env bash
set -euo pipefail

# Build distribution packages for ghcup-gtk with fpm.
#
# Usage: scripts/package.sh -v <version>|head [format...]
#   -v: mandatory; version label used in the package file name; either
#       a numeric version (e.g. 1.2.3) or the literal "head".
#   formats: deb rpm pacman flatpak (Linux), osxpkg (macOS)
#   No format arguments: build every fpm format native to the host OS
#   (flatpak is only built when requested explicitly, since it needs
#   flatpak-builder and the GNOME runtime installed).
#
# Output lands in dist-package/out/.
#
# Make sure you keep the usage of the tools (tar, etc) compatible
# with both GNU and macOS.

usage() {
  echo "usage: scripts/package.sh -v <version>|head [format...]"
  echo "  -v   mandatory; version label for the package file name:"
  echo "       a numeric version (e.g. 1.2.3) or \"head\""
  echo "  formats: deb rpm pacman flatpak (Linux), osxpkg (macOS)"
}

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OS="$(uname -s)"

VERSION_LABEL=""
while getopts ":v:h" opt; do
  case "$opt" in
    v) VERSION_LABEL="$OPTARG" ;;
    h)
      usage
      exit 0
      ;;
    :)
      echo "error: option -$OPTARG requires an argument" >&2
      usage >&2
      exit 1
      ;;
    \?)
      echo "error: unknown option -$OPTARG" >&2
      usage >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND - 1))

if [ -z "$VERSION_LABEL" ]; then
  echo "error: -v is mandatory" >&2
  usage >&2
  exit 1
fi

if [ "$VERSION_LABEL" != "head" ] \
  && ! [[ "$VERSION_LABEL" =~ ^[0-9]+(\.[0-9]+)*$ ]]; then
  echo "error: -v expects a numeric version (e.g. 1.2.3) or \"head\", got '$VERSION_LABEL'" >&2
  exit 1
fi

if [ "$#" -gt 0 ]; then
  FORMATS=("$@")
elif [ "$OS" = "Darwin" ]; then
  FORMATS=(osxpkg)
else
  FORMATS=(deb rpm pacman)
fi

for fmt in "${FORMATS[@]}"; do
  case "$OS,$fmt" in
    Darwin,osxpkg | Linux,deb | Linux,rpm | Linux,pacman | Linux,flatpak) ;;
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

ARCH="$(uname -m)"

# Minimum supported OS per package format. Keep in sync with the
# runners in .github/workflows/release.yml.
min_os_for() {
  case "$1" in
    deb) echo ubuntu-26 ;;
    rpm) echo fedora-44 ;;
    pacman) echo archlinux ;;
    osxpkg) echo macos-15 ;;
  esac
}

echo "==> Building ghcup-gtk ${VERSION}"
CONFIGURE_FLAGS=(--project-file=cabal.release.project --datadir="${PREFIX}/share" --datasubdir=ghcup-gtk)
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
install_file 644 data/icons/funnel-symbolic.svg "${STAGING}${PREFIX}/share/ghcup-gtk/data/icons/funnel-symbolic.svg"

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
  -f
)

build_flatpak() {
  local pkg_name="$1"
  echo "==> flatpak-builder ($pkg_name)"
  flatpak-builder --force-clean --user --install-deps-from=flathub \
    --repo=dist-package/flatpak-repo \
    dist-package/flatpak-build \
    flatpak/org.haskell.GhcupGtk.yml
  flatpak build-bundle dist-package/flatpak-repo \
    "dist-package/out/${pkg_name}" org.haskell.GhcupGtk
}

for fmt in "${FORMATS[@]}"; do
  EXTRA=()
  case "$fmt" in
    flatpak)
      build_flatpak "ghcup-gtk-${VERSION_LABEL}-${ARCH}.flatpak"
      continue
      ;;
    deb) EXTRA=(-d libgtk-4-1 -d libadwaita-1-0); EXT=deb ;;
    rpm) EXTRA=(-d gtk4 -d libadwaita); EXT=rpm ;;
    pacman) EXTRA=(-d gtk4 -d libadwaita); EXT=pkg.tar.zst ;;
    osxpkg) EXTRA=(--osxpkg-identifier-prefix org.haskell); EXT=pkg ;;
  esac
  PKG_NAME="ghcup-gtk-${VERSION_LABEL}-$(min_os_for "$fmt")-${ARCH}.${EXT}"
  echo "==> fpm -t $fmt ($PKG_NAME)"
  fpm "${FPM_COMMON[@]}" -t "$fmt" -p "dist-package/out/${PKG_NAME}" "${EXTRA[@]}" .
done

echo "==> Packages:"
ls -l dist-package/out/
