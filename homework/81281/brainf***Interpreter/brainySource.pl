token(tQuit) --> "quit", !, write("Bye :)\n"), fail.
token(tNextCellAddress) --> ">", !.
token(tLastCellAddress) --> "<", !.
token(tPlusPlusCellContent) --> "+", !.
token(tMinusMinusCellContent) --> "-", !.
token(tWriteCellContent) --> ".", !.
token(tReadCellContent) --> ",", !.
token([tWhileLoop|Body]) --> "[", parceLoop(Body), "]".
token(tUnknown) --> allThatIsNotAToken(I), !, {I \= []}.

parceLoop([]) --> "".
parceLoop([H|T]) --> token(H), !, parceLoop(T).

allThatIsNotAToken([C|Cs]) --> [C|Cs], \+ token(C), \+ token([C|Cs]),
    allThatIsNotAToken(Cs).
allThatIsNotAToken([]) --> [].

parceBF([H|T]) --> token(H), !, parceBF(T).
parceBF([]) --> [], !.

cleanUnknown([], []):- !.
cleanUnknown([tUnknown|T], R):- cleanUnknown(T, R).
cleanUnknown([H|T], [H|R]):- \+ is_list(H), H \= tUnknown, cleanUnknown(T, R).
cleanUnknown([H|T], R):- is_list(H), cleanUnknown(H, R1), cleanUnknown(T, R2), append([R1], R2, R).

interpret(StreamCodes):-
    phrase(parceBF(CommandsU), StreamCodes),
    cleanUnknown(CommandsU, Commands),
    execute(Commands, [[],[0]]),
    !.


execute([], _).

execute([[tWhileLoop|Body]|Rest], Memory):-
    Memory = [_,[CurrentAddress|_]],
    write(Memory),
    CurrentAddress =\= 0,
    append(Body, [[tWhileLoop|Body]|Rest], NewRest),
    !,
    execute(NewRest, Memory).

execute([[tWhileLoop,_]|Rest], Memory):-
    Memory = [_,[0|_]],
    write(Memory),
    !,
    execute(Rest, Memory).

execute([CurrentCommand|Rest], Memory):-
    executeCommand(CurrentCommand, Memory, UpdatedMemory),
    !,
    execute(Rest, UpdatedMemory).


executeCommand(tNextCellAddress, [PrevAddrs, [CurrAddr|NextAddrs]], [NewPrevAddrs, NewNextAddrs]):-
    append(PrevAddrs, [CurrAddr], NewPrevAddrs),
    ((NextAddrs == [], NewNextAddrs = [0]) ; (NewNextAddrs = NextAddrs)).

executeCommand(tLastCellAddress, [PrevAddrs, CurrAddrs], [MorePrevAddrs, [PrevAddr|CurrAddrs]]):-
    append(MorePrevAddrs, [PrevAddr], PrevAddrs).

executeCommand(tPlusPlusCellContent, CurrentMemory, [PrevAddrs, [NewCurrentCell|RestAddrs]]):-
    CurrentMemory = [PrevAddrs, [CurrentCell|RestAddrs]],
    Temp is CurrentCell + 1,
    ((Temp > 255, NewCurrentCell is 0); (NewCurrentCell = Temp)).

executeCommand(tPlusPlusCellContent, CurrentMemory, [PrevAddrs, [NewCurrentCell|RestAddrs]]):-
    CurrentMemory = [PrevAddrs, [CurrentCell|RestAddrs]],
    Temp is CurrentCell - 1,
    ((Temp < 0, NewCurrentCell is 255); (NewCurrentCell = Temp)).

executeCommand(tWriteCellContent, CurrentMemory, CurrentMemory):-
    CurrentMemory = [_, [CurrentCell|_]],
    char_code(Character, CurrentCell),
    write(Character), !.

executeCommand(tReadCellContent, [PrevAddrs, [_|RestAddrs]], [PrevAddrs, [NL|RestAddrs]]):-
    write("Enter character: "),
    get_code(NewCellContent),
    char_code('\r', NewCellContent),
    char_code('\n', NL),
    get_code(NextCellContent),
    (char_code('\n', NextCellContent) ; put_code(NextCellContent)).

executeCommand(tReadCellContent, [PrevAddrs, [_|RestAddrs]], [PrevAddrs, [NewCellContent|RestAddrs]]):-
    write("Enter character: "),
    get_code(NewCellContent).
