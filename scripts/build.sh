#!/bin/sh

set -e

[ -f scripts/local.sh ] && . scripts/local.sh

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
  src/tools/foundry.native       \
  src/tools/gen_vectors.native   \
  $FOUNDRY_JS
