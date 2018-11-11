token(tNextCellAddress) --> ">", !.
token(tLastCellAddress) --> "<", !.
token(tPlusPlusCellContent) --> "+", !.
token(tMinusMinusCellContent) --> "-", !.
token(tWriteCellContent) --> ".", !.
token(tReadCellContent) --> ",", !.
token([tWhileLoop|Body]) --> "[", parseLoop(Body), "]".
token(tUnknown) --> allThatIsNotAToken(I), !, {I \= []}.

parseLoop([]) --> "".
parseLoop([H|T]) --> token(H), !, parseLoop(T).

allThatIsNotAToken([C|Cs]) --> [C|Cs], \+ token(C), \+ token([C|Cs]),
    allThatIsNotAToken(Cs).
allThatIsNotAToken([]) --> [].

parceBF([H|T]) --> token(H), !, parceBF(T).
parceBF([]) --> [], !.

cleanUnknownAndCountInput([], [], 0):- !.
cleanUnknownAndCountInput([tUnknown|T], R, InputCnt):-
    cleanUnknownAndCountInput(T, R, InputCnt).
cleanUnknownAndCountInput([tReadCellContent|T], [tReadCellContent|R], InputCnt):-
    cleanUnknownAndCountInput(T, R, TInputCnt),
    InputCnt is TInputCnt + 1.
cleanUnknownAndCountInput([H|T], [H|R], InputCnt):-
    \+ is_list(H),
    H \= tUnknown,
    H \= tReadCellContent,
    cleanUnknownAndCountInput(T, R, InputCnt).
cleanUnknownAndCountInput([H|T], R, InputCnt):-
    is_list(H),
    cleanUnknownAndCountInput(H, R1, InputBitH),
    cleanUnknownAndCountInput(T, R2, InputBitT),
    InputCnt is InputBitH + InputBitT,
    append([R1], R2, R).

interpret(StreamCodes):-
    phrase(parceBF(CommandsU), StreamCodes),
    cleanUnknownAndCountInput(CommandsU, Commands, InputCnt),
    ((InputCnt =\= 0,
    write("\nThere are input symbols, so ready to read from a file.\nPlease enter the name of the file:\s"),
    current_stream(0, read, Stream),
    read_line_to_codes(Stream, Read),
    open(Read, read, ReadStream));
    (ReadStream = [])),
    execute(Commands, [[],[0]], ReadStream),
    !.


execute([], _, _).

execute([[tWhileLoop|Body]|Rest], Memory, ReadStream):-
    Memory = [_,[CurrCellContent|_]],
    CurrCellContent =\= 0,
    append(Body, [[tWhileLoop|Body]|Rest], NewRest),
    % write("While loop not 0: "),
    % write(NewRest), nl,
    !,
    execute(NewRest, Memory, ReadStream).

execute([[tWhileLoop|_]|Rest], Memory, ReadStream):-
    Memory = [_,[CurrCellContent|_]],
    CurrCellContent =:= 0,
    !,
    % write("While loop is 0: "),
    % write(Rest), nl,
    execute(Rest, Memory, ReadStream).

execute([tReadCellContent|Rest], Memory, ReadStream):-
    executeCommand(tReadCellContent, Memory, UpdatedMemory, ReadStream, ResultStream),
    !,
    execute(Rest, UpdatedMemory, ResultStream).

execute([tReadCellContent|Rest], Memory, []):-
    !,
    execute(Rest, Memory, []).

execute([CurrentCommand|Rest], Memory, ReadStream):-
    executeCommand(CurrentCommand, Memory, UpdatedMemory),
    !,
    execute(Rest, UpdatedMemory, ReadStream).


executeCommand(tNextCellAddress, [PrevAddrs, [CurrAddr|NextAddrs]], [NewPrevAddrs, NewNextAddrs]):-
    append(PrevAddrs, [CurrAddr], NewPrevAddrs),
    ((NextAddrs == [], NewNextAddrs = [0]) ; (NewNextAddrs = NextAddrs)).
    % write("Operation: "), write(tNextCellAddress),
    % write(" Old memory: "), write([PrevAddrs, [CurrAddr|NextAddrs]]),
    % write(" New memory: "), write([NewPrevAddrs, NewNextAddrs]),
    % nl.

executeCommand(tLastCellAddress, [PrevAddrs, CurrAddrs], [MorePrevAddrs, [PrevAddr|CurrAddrs]]):-
    append(MorePrevAddrs, [PrevAddr], PrevAddrs).
    % write("Operation: "), write(tLastCellAddress),
    % write(" Old memory: "), write([PrevAddrs, CurrAddrs]),
    % write(" New memory: "), write([MorePrevAddrs, [PrevAddr|CurrAddrs]]),
    % nl.

executeCommand(tPlusPlusCellContent, CurrentMemory, [PrevAddrs, [NewCurrentCell|RestAddrs]]):-
    CurrentMemory = [PrevAddrs, [CurrentCell|RestAddrs]],
    Temp is CurrentCell + 1,
    ((Temp > 255, NewCurrentCell is 0); (NewCurrentCell is Temp)).
    % write("Operation: "), write(tPlusPlusCellContent),
    % write(" New memory: "), write([PrevAddrs, [NewCurrentCell|RestAddrs]]),
    % nl.

executeCommand(tMinusMinusCellContent, CurrentMemory, [PrevAddrs, [NewCurrentCell|RestAddrs]]):-
    CurrentMemory = [PrevAddrs, [CurrentCell|RestAddrs]],
    Temp is CurrentCell - 1,
    ((Temp < 0, NewCurrentCell is 255); (NewCurrentCell is Temp)).
    % write("Operation: "), write(tMinusMinusCellContent),
    % write(" New memory: "), write([PrevAddrs, [NewCurrentCell|RestAddrs]]),
    % nl.

executeCommand(tWriteCellContent, CurrentMemory, CurrentMemory):-
    CurrentMemory = [_, [CurrentCell|_]],
    char_code(Character, CurrentCell),
    write(Character).

executeCommand(tReadCellContent, [PrevAddrs, [_|RestAddrs]], [PrevAddrs, [NewCellContent|RestAddrs]], ReadStream, ReadStream):-
    is_stream(ReadStream),
    get_code(ReadStream, CurrentInput),
    CurrentInput >= 0,
    ((char_code('\r', CurrentInput), char_code('\n', NL), NewCellContent = NL);
    (NewCellContent = CurrentInput)).

executeCommand(tReadCellContent, [PrevAddrs, [CurrCelContent|RestAddrs]], [PrevAddrs, [CurrCelContent|RestAddrs]], ReadStream, []):-
    is_stream(ReadStream),
    get_code(ReadStream, CurrentInput),
    CurrentInput == -1,
    close(ReadStream).
