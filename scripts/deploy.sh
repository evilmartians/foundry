#!/bin/bash

set -e

ocamlbuild -use-ocamlfind foundryWeb.js
(cd web/ && jekyll build)
rsync -aLvz --delete web/_site/ whitequark.org:/var/www/foundry.whitequark.org
