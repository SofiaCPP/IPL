% From [[[a, b], [a, c]], [b, c]] -> [[a, b], [a, c], [b, c]]
% From [[[a, b], [a, c, [a]]] -> [[a, b], [a, c, [a]], [b, c]].
flattenMine([], []):- !.
flattenMine([H|T], R):- flattenElem(H, RH), flattenMine(T, RT), append(RH, RT, R), !.

flattenElem([H|T], [[H|T]]):- \+ is_list(H), !.
flattenElem([H|T], [H|T]):- is_list(H), H = [H1|_], \+ is_list(H1), !.
flattenElem([H|T], [[H|T]]):- is_list(H), H = [H1|_], is_list(H1), !.

identifyControlStructures([], []):- !.
identifyControlStructures([H|T], [NewH|R]):-
    \+ append(_, [[ttypedef|_]|_], H),
    append(_, [[Token|_]|_], H),
    member(Token, [tdo, tfor, twhile, tif, telse, tswitch, tfunction, tstruct]),

    T = [T1|T2],
    append(T11, [[tleftBrace, "{"], [execNL, "\n"]], T1),
    \+ ((member(M, T11), M \= [execTAB, "\t"])),
    gatherBody(T2, [], PackedStructBody, Rest, 1),
    identifyControlStructures(PackedStructBody, Result),
    append([H, T1], Result, NewH), !,
    identifyControlStructures(Rest, R).
identifyControlStructures([H|T], [H|R]):- !,
    identifyControlStructures(T, R).



gatherBody(Rest, Res, Res, Rest, 0):- !.
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, append(H1, [[tleftBrace, "{"]|_], H),
    \+ ((member(M, H1), M \= [execTAB, "\t"])),
    append(Buff, [H], NewBuff), N1 is N + 1, !,
    gatherBody(T, NewBuff, Res, Rest, N1).
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, append(H1, [[trightBrace, "}"]|_], H),
    \+ ((member(M, H1), M \= [execTAB, "\t"])),
    append(Buff, [H], NewBuff), N1 is N - 1, !,
    gatherBody(T, NewBuff, Res, Rest, N1).
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0,
    append(Buff, [H], NewBuff), !,
    gatherBody(T, NewBuff, Res, Rest, N).
