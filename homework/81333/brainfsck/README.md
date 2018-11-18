# Brainfsck interpreter

# How to build

Install [stack](https://docs.haskellstack.org/en/stable/README/) build tool

Run following commands from root of project:

Building:

```
stack build
```

Executing:

```
stack exec brainfsck <file_with_code>
```

# Examples

Examples are in `examples/` folder.

Best ones are: 

* `stack exec brainfsck examples/sierpinski.bf` - outputs sierpinski triangle
* `echo 0123456789 | stack exec brainfsck examples/pretty.bf` - pretty prints numbers
