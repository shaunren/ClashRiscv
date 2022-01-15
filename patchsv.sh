#!/bin/bash

SED=sed
[ "$(uname)" = "Darwin" ] && SED=gsed

$SED -i 's/c\$/cS_/g; s/\\\$/\\_S_/g; s/_\$/_S_/g; s/\([[:alpha:]]\)'"'"'/\1__/g;' systemverilog/ClashRiscv.CPU.topEntity/*.sv

$SED -i 's/{pllLock};/{pllOut, pllLock};/; /assign pllOut = /d; /, \.rst/a\
    , .outclk_0 (pllOut)
' systemverilog/ClashRiscv.CPU.topEntity/cpu.sv
