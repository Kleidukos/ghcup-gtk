#!/usr/bin/env bash
# Regenerate a screenshot of the app.
#
# Usage: scripts/screenshot.sh -v (list|table) -o OUTPUT.png

set -euo pipefail

usage() {
  echo "usage: $0 -v (list|table) -o OUTPUT.png" >&2
  exit 2
}

screenshot_active_window() {
  local out=$1
  if command -v spectacle > /dev/null; then
    spectacle --activewindow --background --nonotify --output "$out"
  elif command -v gnome-screenshot > /dev/null; then
    gnome-screenshot --window --file="$out"
  else
    echo "$0: no supported screenshot tool found (spectacle, gnome-screenshot)" >&2
    exit 1
  fi
}

view=""
out=""
while getopts "v:o:h" opt; do
  case $opt in
    v) view=$OPTARG ;;
    o) out=$OPTARG ;;
    h | *) usage ;;
  esac
done
shift $((OPTIND - 1))

[ $# -eq 0 ] || usage
[ -n "$out" ] || usage
case $view in
  list | table) ;;
  *) usage ;;
esac

bin=$(cabal list-bin exe:ghcup-gtk)

"$bin" --view "$view" &
app_pid=$!
trap 'kill "$app_pid" 2>/dev/null || true' EXIT

sleep 8

screenshot_active_window "$out"

for _ in $(seq 1 20); do
  [ -s "$out" ] && break
  sleep 0.5
done
[ -s "$out" ] || {
  echo "$0: screenshot tool did not write $out" >&2
  exit 1
}

echo "wrote $out"
