:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).
:- [tokens, prettify, print, essentials].
:- set_prolog_flag(verbose, silent).

:- initialization(main, main).

main :-
    current_prolog_flag(argv, Argv),
    Argv = [H|_],
    atom_string(H, File),
    mainy(File),
    format('You can see the result in the html file.~n'),
    halt.
main :-
    halt(1).

mainy(File):-
    open(File, read, RFile),
    read_stream_to_codes(RFile, CSource),
    close(RFile),
    phrase(tokens(Tokens), CSource),
    flattenMine(Tokens, TokenStream),
    prettify(TokenStream, PrettyStream),
    open('page.html', write, WFile),
    write(WFile,"<!doctype>
    <html>
    <head>
        <title>hello.c</title>
        <style>
           .keyword {
               color: blue;
           }
           .number {
               color: violet;
           }
           .identifier {
               color: green;
           }
           .operator {
               font-style: bold;
               color: blue;
           }
           .comment {
               color: lime;
           }
           .string {
               color: orange;
           }
           .parenthese {
               font-style: bold;
               color: brown;
           }
           .quotation {
               font-style: bold;
               color: black;
           }
           .unknown {
               font-style: bold;
               color: red;
           }
           .function {
               color: purple;
           }
       </style>
    </head>
    <body>
        <pre class=\"code\">"),
    tohtmlfile(WFile, PrettyStream),
    close(WFile).
