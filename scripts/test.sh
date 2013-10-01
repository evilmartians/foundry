#!/bin/sh

set -e

[ -f scripts/local.sh ] && . scripts/local.sh

if [ -n "$COVERAGE" ]; then
  find test -name 'bisect*.out' -delete
fi

if [ -z "$LIT_ARGS" ]; then
  LIT_ARGS="-v -s --no-progress-bar"
fi

echo "Testing foundry..."
OCAMLRUNPARAM=Rb ./test_foundry.native; echo

(ulimit -t 5 ; ulimit -d 512000 ; ulimit -m 512000 ; ulimit -s 8192
 ./vendor/lit/lit.py $LIT_ARGS test/)

if [ -n "$COVERAGE" ]; then
  echo "Generating coverage..."
  (cd _build;
   bisect-report -html ../coverage `find ../test -name 'bisect*.out'`;
   find ../test -name 'bisect*.out' -delete)
fi
