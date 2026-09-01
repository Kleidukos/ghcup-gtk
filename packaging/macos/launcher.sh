#!/bin/sh
set -eu

CONTENTS="$(CDPATH='' cd -P "$(dirname "$0")/.." && pwd)"
RESOURCES="${CONTENTS}/Resources"

ghcup_gtk_datadir="${RESOURCES}/share/ghcup-gtk"
GSETTINGS_SCHEMA_DIR="${RESOURCES}/share/glib-2.0/schemas"
GDK_PIXBUF_MODULE_FILE="${RESOURCES}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"
XDG_DATA_DIRS="${RESOURCES}/share"
export ghcup_gtk_datadir GSETTINGS_SCHEMA_DIR GDK_PIXBUF_MODULE_FILE XDG_DATA_DIRS

exec "${CONTENTS}/MacOS/ghcup-gtk-bin" "$@"
