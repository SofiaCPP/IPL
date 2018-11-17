% From [[[a, b], [a, c]], [b, c]] -> [[a, b], [a, c], [b, c]]
% From [[[a, b], [a, c, [a]]] -> [[a, b], [a, c, [a]], [b, c]].
flattenMine([], []):- !.
flattenMine([H|T], R):- flattenElem(H, RH), flattenMine(T, RT), append(RH, RT, R), !.

flattenElem([H|T], [[H|T]]):- \+ is_list(H), !.
flattenElem([H|T], [H|T]):- is_list(H), H = [H1|_], \+ is_list(H1), !.
flattenElem([H|T], [[H|T]]):- is_list(H), H = [H1|_], is_list(H1), !.

getPrefixTabsLength(L, N):- append(A, B, L),
    \+((member(AA, A), AA \= [execTAB, "\s\s\s\s"])), \+((member(BB, B), BB == [execTAB, "\s\s\s\s"])), length(A, N).

removeNTabs(L, 0, L):- !.
removeNTabs([_|T], N, R):- N > 0, N1 is N - 1, !, removeNTabs(T, N1, R).
