#!/bin/sh

set -e

SRC=$1
FIR=$(basename $SRC .fy).fir
OPT=$(basename $SRC .fy).opt.fir
OBJ=$(basename $SRC .fy).o
EXE=$(basename $SRC .fy).f

rm -f $FIR $OPT $OBJ $EXE

./foundry_vm.native   $SRC -o $FIR
./foundry_xfrm.native $FIR -std-xfrms -o $OPT && rm $FIR
./foundry_gen.native  $OPT | llc-3.4 -filetype=obj -o $OBJ && rm $OPT
gcc $OBJ -o $EXE && rm $OBJ

./$EXE
