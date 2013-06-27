#!/bin/bash

set -e

ocamlbuild -use-ocamlfind foundry_web.byte
js_of_ocaml foundry_web.byte -o web/foundry.js
