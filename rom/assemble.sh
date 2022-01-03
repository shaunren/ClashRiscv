#!/bin/bash

riscv64-unknown-elf-as -march=rv32im test.s
riscv64-unknown-elf-objcopy -O binary a.out rom.bin
rm a.out
