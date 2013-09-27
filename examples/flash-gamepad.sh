#!/bin/sh

set -e
if [ "$1" = "" ]; then
  echo "Usage: $0 [project name]"
  exit 1
fi

CROSS=arm-linux-gnueabi-
PROJECT=$1

make ${PROJECT}.d.elf ${PROJECT}.d.ihex
gdbflasher --mcu=stm32l1xx -t ${PROJECT}.d.ihex
${CROSS}gdb ${PROJECT}.d.elf \
  -ex 'tar rem localhost:2331' \
  -ex 'set $sp = *0x08000000'  \
  -ex 'set $pc = *0x08000004'  \
  -ex 'cont'
