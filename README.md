<!-- omit in toc -->
# ClashRiscv

A simple in-order 5 stage RV32IM CPU implemented in Clash.

## Features

- Supports the RV32IM instruction set
- Single issue
- 5 stages (IF -> ID -> EX -> MEM -> WB)
- 3 cycle pipelined multiplier
- 17 cycle divider
- Synthesizes on Cyclone V with Fmax = 91.1 MHz (slow 100C model)
