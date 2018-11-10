:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).
:- [brainySource].

:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

main:-
    current_prolog_flag(argv, Argv),
    Argv = [H1|_],
    atom_string(H1, Filename),
    ((Filename == "", readyToReadAndInterpret("Hello, how can I help you?"));
    (Filename \= "", readyToReadAndInterpret("Ready to interpret your file.\n", Filename))),
    halt.
main:-
    halt(1).

readyToReadAndInterpret(Message):-
    write(Message),
    startCycleTillQuit.

readyToReadAndInterpret(Message, File):-
    write(Message),
    open(File, read, RFile),
    read_stream_to_codes(RFile, BFSource),
    close(RFile),
    interpret(BFSource).

startCycleTillQuit:-
    nl,
    current_stream(0, read, Stream),
    read_line_to_codes(Stream, BFProgram),
    interpret(BFProgram),
    startCycleTillQuit.
