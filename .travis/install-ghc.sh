#!/bin/sh
set -eux
#assume stack is installed
STACK=~/.local/bin/stack

fetch_ghc_osx() {
  echo "Using stack to install GHC $GHC_VERSION"
  $STACK setup $GHC_VERSION
}

if [ "$(uname)" = "Darwin" ]; then
  fetch_ghc_osx
else
  #installed by apt
  echo "Linux GHC $GHC_VERSION via APT already installed."
fi
