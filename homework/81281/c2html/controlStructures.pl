:- [essentials, tokens].
identifyControlStructures([], []):- !.
identifyControlStructures([H|T], [NewH|R]):-
    \+ append(_, [[ttypedef|_]|_], H),
    append(_, [[Token|_]|_], H),
    member(Token, [tdo, tfor, twhile, tif, telse, tswitch, tfunction, tstruct]),
    T = [T1|T2],
    append(T11, [[tleftBrace, "{"], [execNL, "\n"]], T1),
    \+ ((member(M, T11), M \= [execTAB, "\s\s\s\s"])),
    gatherBody(T2, [], PackedStructBody, Rest, 1),
    identifyControlStructures(PackedStructBody, Result),
    append([H, T1], Result, NewH), !,
    identifyControlStructures(Rest, R).
identifyControlStructures([H|T], [NewH|R]):-
    \+ append(_, [[ttypedef|_]|_], H),
    append(_, [[Token|_]|_], H),
    member(Token, [tfor, twhile, tif, telse]),
    T = [T1|_],
    \+ append(_, [[tleftBrace, "{"], [execNL, "\n"]], T1),
    getPrefixTabsLength(H, N),
    getPrefixTabsLength(T1, M),
    N + 1 =:= M,
    gatherOneLineBody(T, [], PackedOneLineBody, Rest, N),
    identifyControlStructures(PackedOneLineBody, Result),
    append([H], Result, NewH), !,
    identifyControlStructures(Rest, R).
identifyControlStructures([H|T], [H|R]):- !,
    identifyControlStructures(T, R).


gatherOneLineBody([], Res, Res, [], _):- !.
gatherOneLineBody([H|T], Res, Res, [H|T], N):-
    getPrefixTabsLength(H, M),
    N + 1 =\= M, !.
gatherOneLineBody([H|T], Buff, Res, Rest, N):-
    getPrefixTabsLength(H, M),
    N + 1 =:= M,
    append(Buff, [H], NewBuff), !,
    gatherOneLineBody(T, NewBuff, Res, Rest, M).


gatherBody(Rest, Res, Res, Rest, 0):- !.
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, append(H1, [[tleftBrace, "{"]|_], H),
    \+ ((member(M, H1), M \= [execTAB, "\s\s\s\s"])),
    append(Buff, [H], NewBuff), N1 is N + 1, !,
    gatherBody(T, NewBuff, Res, Rest, N1).
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, append(H1, [[trightBrace, "}"]|_], H),
    \+ ((member(M, H1), M \= [execTAB, "\s\s\s\s"])),
    append(Buff, [H], NewBuff), N1 is N - 1, !,
    gatherBody(T, NewBuff, Res, Rest, N1).
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0,
    append(Buff, [H], NewBuff), !,
    gatherBody(T, NewBuff, Res, Rest, N).
