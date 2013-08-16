#!/bin/sh

set -e

if ! [ -x vendor/_prefix/bin/menhir ]; then
  echo "Building menhir..."
  (cd vendor/menhir;
   # Fails on manual.pdf or something.
   make PREFIX=$(pwd)/../_prefix/ install || true;
   git clean -dxf >/dev/null 2>&1)
fi

if ! [ -x vendor/merr/merr.native ]; then
  echo "Building merr..."
  (cd vendor/merr;
   export MENHIR=$(pwd)/../_prefix/bin/menhir;
   ocamlbuild -use-ocamlfind merr/merr.native;
   ocamlbuild -use-ocamlfind merr/merr.native)
fi

echo "Building foundry..."
export OCAMLPATH=/usr/lib/ocaml/llvm-3.4/
ocamlbuild -j 8 -use-ocamlfind file_check.native foundry_as.native foundry_opt.native #foundry_web.js

echo "Testing foundry..."
./vendor/lit/lit.py -v test/
