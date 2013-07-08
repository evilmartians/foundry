#!/bin/bash

set -e

cd menhir
make PREFIX="$(pwd)/../menhir-bin" install || true
git clean -dxf
