#!/bin/sh

set -e

[ -f scripts/local.sh ] && . scripts/local.sh

if ! [ -x vendor/_prefix/bin/menhir ]; then
  echo "Building menhir..."
  (cd vendor/menhir;
   touch manual.pdf; # missing for some reason
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

echo "Building foundry..."

if [ -n "$JS_OF_OCAML" ]; then
  FOUNDRY_JS=foundry_web.js
fi

ocamlbuild -j 8 -use-ocamlfind \
  unittest/test_foundry.native  \
  src/tools/file_check.native    \
  src/tools/not.native           \
  src/tools/foundry_vm.native    \
  src/tools/foundry_xfrm.native  \
  src/tools/foundry_gen.native   \
  $FOUNDRY_JS
