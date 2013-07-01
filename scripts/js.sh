#!/bin/bash

set -e

ocamlbuild -use-ocamlfind foundryWeb.byte
js_of_ocaml foundryWeb.byte -o web/foundry.js
