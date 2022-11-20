# Ruby

## Grammar
program := statement, [nl, program]
statement := call | variableif  | function | conditional
call := (literal | identifier), [function_chain], [block]
function_chain := ., identifier[, (, arguments ,)], [function_chain]
block := do, [|, arguments, |], nl, program, end
arguments := variable, [, arguments]
function := def, identifier, [(, arguments, )], nl, program, end |
            class, identifier, [< identifier], nl, program, end |
            module, identifier, nl, program, end
conditional := if, conditions, nl, program, [elsif_conditional], [else, nl, program], end
elsif_conditional := elsif, condition, nl, program, [elsif_conditional]
nl := \n
variable := identifier[, =, literal]
literal := string | number | symbol | array | hash
conditions := condition, [(&& | ||), condtitions]
condition := lhs, op, rhs

## Build Examples
To build a single example one must enter the `/examples` folder and run the `build.sh` script, by providing the example name.
To build all the examples, just run the script without any arguments.

## To Do
- Tests
- Imporve Code Readability/Complexity
