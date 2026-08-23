#!/usr/bin/env sh
set -euo pipefail
PREFIX="${HOME}/.local"
mkdir -p "${PREFIX}/bin" "${PREFIX}/share/applications"
install -m 755 ghcup-gtk "${PREFIX}/bin/ghcup-gtk"
install -m 644 org.haskell.GhcupGtk.desktop "${PREFIX}/share/applications/org.haskell.GhcupGtk.desktop"
update-desktop-database "${PREFIX}/share/applications" 2>/dev/null || true
echo "Installed into ${PREFIX}. Log out and back in if the app does not appear in your launcher."
