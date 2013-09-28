#!/bin/sh

set -e
if [ "$1" = "" ]; then
  echo "Usage: $0 [project name]"
  exit 1
fi

PROJECT=$1

make ${PROJECT}.d.bin
dfu-util -R -a 1 -D ${PROJECT}.d.bin
