# Brainfuck-Interpreter
Simple Brainfuck interpreter written for IPL course homework from FMI, SU, 2018.

Uses default memory size of 30000 cells but if you define `#MEMSIZE` with your compiler to something else you can get different size.

When either > or < instructions overflow the cell pointer, the latter just wraps
around.

# Installation
BFI uses _make_ for make for simplicity.

To install simply run:
```
make
```

# Usage
```
Usage: bfi [OPTIONS]

Options:
  -h,--help                   Print this help message and exit
  -f,--file FILE Excludes: --code
                              Brainfuck source file
  -c,--code TEXT Excludes: --file
                              Brainfuck source code
  -o,--output TEXT            File to write the Brainfuck output to. Optional.
  -i,--input FILE             File to take the Brainfuck input from. Optional.
```

# Future
BFI is planned to be made child-friendly by introducing new instructions like:

* ! - make the current cell value equal to 0.
* ^ - go to cell No. (current cell No. + current cell value).
* & - go to cell No. (current cell No. - current cell value).
* \# - if statement that depends on next value (for comparison) and previous (to use like ^)
* $ - same as above, but go back, instead of forward.

To be completely child-friendly change the name of the language to Brainmeddler.
