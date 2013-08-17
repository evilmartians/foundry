#!/bin/sh

set -e

echo "Testing foundry..."
./vendor/lit/lit.py -v test/
