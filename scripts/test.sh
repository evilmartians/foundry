#!/bin/sh

set -e

echo "Testing foundry..."
./vendor/lit/lit.py -v test/

if [ -n "$COVERAGE" ]; then
  echo "Generating coverage..."
  (cd _build;
   bisect-report -html ../coverage `find ../test -name 'bisect*.out'`;
   find ../test -name 'bisect*.out' -delete)
fi
