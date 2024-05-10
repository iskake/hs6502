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

## Assembling and running programs

Example programs are available in the `examples/` folder. To assemble them, simply run the assembler on every file:

```sh
~ $ cabal -O2 run assembler examples/*.s
```

And run them as was shown above.

## Creating your own programs

You can also create and assemble your own programs.

todo... check out https://www.nesdev.org/obelisk-6502-guide/ if you are actually interested in doing this.


## Useful and helpful sources

- The [Nesdev Wiki](https://www.nesdev.org/wiki/Nesdev_Wiki), especially the [CPU section](https://www.nesdev.org/wiki/CPU) and [obelisk 6502 reference](https://www.nesdev.org/obelisk-6502-guide/)
- Including external links found on the wiki or otherwise:
    - [6502 opcode matrix](https://www.oxyron.de/html/opcodes02.html)
    - [6502 instruction set decoding from opcodes](https://llx.com/Neil/a2/opcodes.html#ins02)
- The [Mesen](https://www.github.com/SourMesen/Mesen2) emulator, especially it's assembler and extensive debugger
