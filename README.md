# ghcup-gtk

A GTK4/libadwaita installer for the Haskell toolchain.

It aims to have feature parity with the GHCup TUI.

## Development

### System Dependencies

_Please open a ticket if these are not up to date_

#### Fedora

```bash
$ sudo dnf install gobject-introspection-devel glib2-devel gtk4-devel libadwaita-devel \
  pango-devel cairo-gobject-devel harfbuzz-devel graphene-devel freetype-devel \
  gdk-pixbuf2-devel zlib-ng-compat-devel xz-devel pkgconf-pkg-config gcc make
```

#### Ubuntu/Debian


```bash
sudo apt install build-essential pkg-config libgirepository1.0-dev libglib2.0-dev \
  libgtk-4-dev libadwaita-1-dev libpango1.0-dev libcairo2-dev libharfbuzz-dev \
  libgraphene-1.0-dev libfreetype-dev libgdk-pixbuf-2.0-dev zlib1g-dev liblzma-dev
```


#### Arch


```bash
sudo pacman -S --needed base-devel pkgconf gobject-introspection glib2-devel gtk4 \
  libadwaita pango cairo harfbuzz graphene freetype2 gdk-pixbuf2 zlib xz
```


#### Homebrew

```bash
brew install pkgconf gobject-introspection glib gtk4 libadwaita pango cairo \
  harfbuzz graphene freetype gdk-pixbuf zlib xz
```

### Commands

- `make run`: compile and start the application
- `make test`: run the test suite
- `make dist`: build the release tarball
