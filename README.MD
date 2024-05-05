# hs6502

A 6502 CPU emulator written in haskell.

The hs6502 repository is divided into three main parts:

- the HS6502 library, the CPU emulator itself (in `src/`)
- a basic assembler to create binary programs (in `assembler/`)
- a basic "fantasy computer" with 32KiB/32KiB of RAM/ROM (respectively) for running assembled programs (in `app/`)

## Usage

To run the emulator use `cabal build` to build each part, or simply use `cabal run`:

``` sh
# Assemble the 6502 assembly file 'program.s' into a binary program
~ $ cabal -O2 run assembler program.s

# Run the resulting binary program 'program.bin'
~ $ cabal -O2 run hs6502 program.bin
```

## more...

todo todo todo....

## Useful sources

- The [Nesdev Wiki](https://www.nesdev.org/wiki/Nesdev_Wiki), especially the [CPU section](https://www.nesdev.org/wiki/CPU) and [obelisk 6502 reference](https://www.nesdev.org/obelisk-6502-guide/)