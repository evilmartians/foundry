#!/bin/sh

set -e

if [ -n "$COVERAGE" ]; then
  find test -name 'bisect*.out' -delete
fi

echo "Testing foundry..."
./vendor/lit/lit.py -v -s --no-progress-bar test/

if [ -n "$COVERAGE" ]; then
  echo "Generating coverage..."
  (cd _build;
   bisect-report -html ../coverage `find ../test -name 'bisect*.out'`;
   find ../test -name 'bisect*.out' -delete)
fi
