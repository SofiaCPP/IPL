% From [[[a, b], [a, c]], [b, c]] -> [[a, b], [a, c], [b, c]]
% From [[[a, b], [a, c, [a]]] -> [[a, b], [a, c, [a]], [b, c]].
flattenMine([], []):- !.
flattenMine([H|T], R):- flattenElem(H, RH), flattenMine(T, RT), append(RH, RT, R), !.

flattenElem([H|T], [[H|T]]):- \+ is_list(H), !.
flattenElem([H|T], [H|T]):- is_list(H), H = [H1|_], \+ is_list(H1), !.
flattenElem([H|T], [[H|T]]):- is_list(H), H = [H1|_], is_list(H1), !.

identifyFunctionsAndStructures([], []):- !.
identifyFunctionsAndStructures([H|T], [NewH|R]):-
    \+ append(_, [[ttypedef|_]|_], H),
    append(_, [[tstruct|_]|_], H),
    T = [T1|T2],
    T1 == [[tleftBrace, "{"], [execNL, "\n"]],
    gatherBody(T2, [], PackedStructBody, Rest, 1),
    append([H, T1], PackedStructBody, NewH), !,
    identifyFunctionsAndStructures(Rest, R).
identifyFunctionsAndStructures([H|T], [NewH|R]):-
    append(_, [[tfunction|_]|_], H),
    T = [T1|T2],
    T1 == [[tleftBrace, "{"], [execNL, "\n"]],
    gatherBody(T2, [], PackedFunctionBody, Rest, 1),
    append([H, T1], PackedFunctionBody, NewH), !,
    identifyFunctionsAndStructures(Rest, R).
identifyFunctionsAndStructures([H|T], [H|R]):- !,
    identifyFunctionsAndStructures(T, R).

gatherBody(Rest, Res, Res, Rest, 0):- !.
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, H = [[H11|_]|_], H11 == tleftBrace, !,
    append(Buff, [H], NewBuff), N1 is N + 1,
    gatherBody(T, NewBuff, Res, Rest, N1).
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, H = [[H11|_]|_], H11 == trightBrace, !,
    append(Buff, [H], NewBuff), N1 is N - 1, !,
    gatherBody(T, NewBuff, Res, Rest, N1).
gatherBody([H|T], Buff, Res, Rest, N):-
    N =\= 0,
    append(Buff, [H], NewBuff), !,
    gatherBody(T, NewBuff, Res, Rest, N).
