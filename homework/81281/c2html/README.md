# Homework 1 + 2
## Create a xxx2html syntax highlighter for a language of your choice. 
+ ~~In this case it is C.~~
## Extend the xxx2html tool to:

+ ~~Produce pretty-printed html.~~(Still testing, but almoast finished.)

+ **TODO:** bonus: support line length limit

+ **TODO:** Produce html where functions have their background colored depending on cyclomatic complexity ranging from light green to light read.

+ **TODO:** Produce html where control structures can be collapsed.

### How to install and use SWI-Prolog

+ [How to install SWI-Prolog](https://wwu-pi.github.io/tutorials/lectures/lsp/010_install_swi_prolog.html)

+ [Quick start on how to use it](http://www.swi-prolog.org/pldoc/man?section=quickstart)

+ [Command line options](http://www.swi-prolog.org/pldoc/man?section=cmdline)

### Added bash script for executing program after successful installation of SWI-Prolog on a Unix or Unix-like variants
[Script](./c2htmlrun.sh)

### If you are going to install SWI-Prolog on Windows 
+ You can omit lines 6 to 16 from c2html.pl.
+ Afterward in order to execute the program you can call the main predicate and give it a valid string name of file.

For example **mainy("test.c").**

### For the curious who want to find about DCG Grammar rules :

+ [SWI-Prolog/DCG](http://www.swi-prolog.org/pldoc/man?section=DCG)

+ [Wikibooks](https://en.wikibooks.org/wiki/Prolog/Definite_Clause_Grammars)

+ [A tutorial](http://www.pathwayslms.com/swipltuts/dcg/)
