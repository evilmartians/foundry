#!/bin/bash

set -e

(cd menhir;
 make PREFIX="$(pwd)/../menhir-bin";
 make PREFIX="$(pwd)/../menhir-bin" install || true;
 git clean -dxf)

(cd merr;
 export MENHIR="$(pwd)/../menhir-bin/bin/menhir";
 ocamlbuild -use-ocamlfind merr.native;
 ocamlbuild -use-ocamlfind merr.native)
