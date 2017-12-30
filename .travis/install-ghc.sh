#!/bin/sh
set -eux
#assume stack is installed
STACK=~/.local/bin/stack

fetch_ghc_osx() {
  echo "Using stack to install GHC $GHCVER"
  $STACK setup $GHCVER
}

if [ "$(uname)" = "Darwin" ]; then
  fetch_ghc_osx
else
  #installed by apt
  echo "Linux GHC via APT already installed."
fi
