#!/usr/bin/env bash

set -ex

if [ -e "$HOME"/lib/libblosc.so ]; then
  echo "Found existing libblosc; skipping install:"
  ls -l "$HOME"/lib/*blosc*
  exit 0
fi

which cmake
# sudo apt-get update -qq
# sudo apt-get install cmake
git clone https://github.com/Blosc/c-blosc.git
cd c-blosc
rm -rf build
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX="$HOME" ..
cmake --build . --target install
