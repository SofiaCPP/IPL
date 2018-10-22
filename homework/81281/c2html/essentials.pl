% From [[[a, b], [a, c]], [b, c]] -> [[a, b], [a, c], [b, c]]
% From [[[a, b], [a, c, [a]]] -> [[a, b], [a, c, [a]], [b, c]].
flattenMine([], []):- !.
flattenMine([H|T], R):- flattenElem(H, RH), flattenMine(T, RT), append(RH, RT, R), !.

flattenElem([H|T], [[H|T]]):- \+ is_list(H), !.
flattenElem([H|T], [H|T]):- is_list(H), H = [H1|_], \+ is_list(H1), !.
flattenElem([H|T], [[H|T]]):- is_list(H), H = [H1|_], is_list(H1), !.

identifyFunctions([], []):- !.
identifyFunctions([H|T], [NewH|R]):-
    append(_, [[tfunction|_]|_], H),
    T = [T1|T2],
    T1 == [[tleftBrace, "{"], [execNL, "\n"]],
    gatherFunctionBody(T2, [], PackedFunctionBody, Rest, 1),
    append([H, T1], PackedFunctionBody, NewH), !,
    identifyFunctions(Rest, R).
identifyFunctions([H|T], [H|R]):- !,
    identifyFunctions(T, R).

gatherFunctionBody(Rest, Res, Res, Rest, 0):- !.
gatherFunctionBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, H == [[tleftBrace, "{"], [execNL, "\n"]], !,
    append(Buff, [H], NewBuff), N1 is N + 1,
    gatherFunctionBody(T, NewBuff, Res, Rest, N1).
gatherFunctionBody([H|T], Buff, Res, Rest, N):-
    N =\= 0, H == [[trightBrace, "}"], [execNL, "\n"],[execNL, "\n"]], !,
    append(Buff, [H], NewBuff), N1 is N - 1, !,
    gatherFunctionBody(T, NewBuff, Res, Rest, N1).
gatherFunctionBody([H|T], Buff, Res, Rest, N):-
    N =\= 0,
    append(Buff, [H], NewBuff), !,
    gatherFunctionBody(T, NewBuff, Res, Rest, N).
