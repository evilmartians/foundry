#!/bin/sh

set -e

if ! [ -x vendor/_prefix/bin/menhir ]; then
  echo "Building menhir..."
  (cd vendor/menhir;
   touch manual.pdf; # missing for some reason
   ocamlfind remove menhirLib >/dev/null 2>&1 || true;
   make PREFIX=$(pwd)/../_prefix/ install;
   git clean -dxf >/dev/null 2>&1)
fi

if ! [ -x vendor/merr/merr.native ]; then
  echo "Building merr..."
  (cd vendor/merr;
   export MENHIR=$(pwd)/../_prefix/bin/menhir;
   ocamlbuild -use-ocamlfind merr/merr.native;
   ocamlbuild -use-ocamlfind merr/merr.native)
fi

opam install sexplib extlib ulex ounit batteries js_of_ocaml bisect
