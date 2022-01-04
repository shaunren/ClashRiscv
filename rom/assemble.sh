#!/bin/bash

riscv64-unknown-elf-as -march=rv32im test.s
riscv64-unknown-elf-objcopy -O binary a.out rom.o
stack exec createROM 1024 < rom.o > rom.bin
rm a.out
