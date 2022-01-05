#!/bin/bash

sed -i '' 's/{pllLock};/{pllOut, pllLock};/; /assign pllOut = /d; /, \.rst/a\
    , .outclk_0 (pllOut)
' systemverilog/ClashRiscv.CPU.topEntity/cpu.sv
