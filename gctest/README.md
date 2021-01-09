# GC Test Bench

A program to stress test garbage collector implementations.

## Building

The program is implemented as a static library, that has to be linked together
with the garbage collector implementation to produce a test program.

## Adding a new Garbage Collector implementation

1. Implement the `GarbageCollector` interface from `gc.h` and the factory
   function
2. Add a new target in the `genie.lua` project

## Running

Running the program produces a `program.json` that can be open in
`chrome://tracing` to allow analysis of the mutator vs garbage collector CPU
usage and memory usage.
 
