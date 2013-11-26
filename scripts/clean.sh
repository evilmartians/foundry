#!/bin/sh

set -e

[ -f scripts/local.sh ] && . scripts/local.sh

echo "Cleaning foundry..."

ocamlbuild -clean
