#!/usr/bin/env bash

set -euo pipefail

echo "Setting up the environment for linux"
sudo apt-get update
sudo apt install -y libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
