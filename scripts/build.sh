#!/bin/sh

set -e

if ! [ -x menhir-bin/bin/menhir ]; then
  echo "Building menhir..."
  (cd menhir;
   # Fails on manual.pdf or something.
   make PREFIX=$(pwd)/../menhir-bin/ install || true;
   git clean -dxf)
fi

if ! [ -x merr/merr.native ]; then
  echo "Building merr..."
  (cd merr;
   export MENHIR=$(pwd)/../menhir-bin/bin/menhir;
   ocamlbuild -use-ocamlfind merr/merr.native;
   ocamlbuild -use-ocamlfind merr/merr.native)
fi

echo "Building foundry..."
export OCAMLPATH=/usr/lib/ocaml/llvm-3.4/
ocamlbuild -j 8 -use-ocamlfind file_check.native foundry_as.native foundry_opt.native #foundry_web.js

echo "Testing foundry..."
./lit/lit.py -v test/
