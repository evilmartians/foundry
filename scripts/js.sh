#!/bin/bash

set -e

ocamlbuild -use-ocamlfind foundryWeb.byte
js_of_ocaml foundryWeb.byte -o web/js/foundry.js
