#!/bin/bash

PREFIX=riscv64-linux-gnu
[ "$(uname)" = "Darwin" ] && PREFIX=riscv64-unknown-elf

$PREFIX-as -march=rv32im "$1"
$PREFIX-objcopy -O binary a.out rom.o
stack exec createROM 1024 < rom.o > rom.bin
rm a.out
