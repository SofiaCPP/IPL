% From [[[a, b], [a, c]], [b, c]] -> [[a, b], [a, c], [b, c]]
flattenMine([], []):- !.
flattenMine([H|T], R):- flattenElem(H, RH), flattenMine(T, RT), append(RH, RT, R), !.

flattenElem([H|T], [[H|T]]):- \+ is_list(H), !.
flattenElem([H|T], [H|T]):- is_list(H), !.
