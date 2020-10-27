# HW1 81618 - c2html

## Running

There's no test file given, as any self respecting parser should parse its own source code.

c2html outputs HTML to stdout, you'll have to redirect it to a file:

```
./c2html c2html.c > c2html.c.html
firefox c2html.c.html
```

## Compiling

There are C99 features used that won't compile under a C++ compiler, so don't use one.

That aside, the only dependency is libc, and optionally gperf if you want to regenerate the keywords file.

```sh
cc c2html.c -o c2html
```

### Generating the keywords file

keywords.gperf.c is generated via:

```sh
gperf keywords.gperf > keywords.gperf.c
```
