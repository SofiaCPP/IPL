# Ruby

## Grammar
program := (function | conditional | variable | call), \n[, program]
function := def, " ", identifier[, (, arguments, )], \n, program, end
conditional := if, conditions, \n, program, end[, conditional]
variable := identifier, =, identifier
call := identifier[, function_chain][, block]
arguments := identifier[, arguments]
conditions := condition[, logic_operator, conditions]
condition := identifier, compare_operator, identifier
function_chain := ., identifier[, (, arguments, )][, function_chain]
block := do, " "[, |, arguments, |], \n, program, end
logic_operator := or | and
compare_operator := <= | => | == | < | >

## Build Examples
To build a single example one must enter the `/examples` folder and run the `build.sh` script, by providing the example name.
To build all the examples, just run the script without any arguments.

## To Do
- Fix a lot of stuff
- Literals
- Conditionals
- Collapsing
- Loops
- Tags
