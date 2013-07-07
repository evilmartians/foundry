#!/bin/bash

set -e

(cd web/ && jekyll build)
rsync -avz web/_site/ whitequark.org:/var/www/foundry.whitequark.org
