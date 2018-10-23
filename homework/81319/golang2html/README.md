# How-to guide

## Compiling the highlighter

1. Transpile flex file to c file
```
flex golang2html.flex
```

2. Compile the produced output file

```
gcc lex.yy.c -o lexer
```

The produced output is the 'lexer' file. You use it to highlight
your own golang source files.

## Highlighting Golang code

1. Create your own golang source file with whatever name you wish. For
this example, let your golang source be 'hello_world.go'.

2. Produce the highlighted html output

```
./lex hello_world.go > hello_world.html
```

## Enjoy
