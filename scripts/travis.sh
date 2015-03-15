#!/bin/bash

cabal configure -fexamples --enable-tests --enable-benchmarks --enable-coverage --ghc-options="-Wall -Werror"
cabal build -j
cabal test --show-details=always
cabal check
cabal sdist
export SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}')
pushd dist/
if [ -f "$SRC_TGZ" ]; then
  cabal install "$SRC_TGZ"
else
  echo "expected '$SRC_TGZ' not found"
  exit 1
fi
popd
