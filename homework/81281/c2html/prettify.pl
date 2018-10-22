:- [essentials].


prettify(StreamTokens, PrettyStream):-
    removeAllWhites(StreamTokens, Plain),
    % write(Plain),nl,write("Plain ------------"), nl,
    addNewLines(Plain, PlainWithNL),
    % write(PlainWithNL),nl,write("PlainWithNL ------------"), nl,
    addSpaces(PlainWithNL, PlainWithNLnS),
    % write(PlainWithNLnS),nl,write("PlainWithNLnS ------------"), nl,
    addTabs(PlainWithNLnS, PlainWithNLnSnT),
    % write(PlainWithNLnSnT),nl,write("PlainWithNLnSnT ------------"), nl,
    flattenMine(PlainWithNLnSnT, PrettyStream).
    % write(PrettyStream), nl.


%% Removes all white charachters
removeAllWhites([], []):- !.
removeAllWhites([[H,_]|T], R):- member(H, [execTAB, tspace, execNL]),
    removeAllWhites(T, R), !.
removeAllWhites([[H, B]|T], [[H, B]|R]):- \+ member(H, [execTAB, tspace, execNL]),
    removeAllWhites(T, R), !.


%% Pack for while, if, for and function arguments
packTillRightParen(Rest, Buff, Res, Rest, 0):-
    append(Buff, [[execNL, '\n']], Res).
packTillRightParen([H|T], Buff, Res, Rest, N):-
    N =\= 0, H = [H1|_], H1 == tleftParen,
    append(Buff, [H], NewBuff), N1 is N + 1,
    packTillRightParen(T, NewBuff, Res, Rest, N1).
packTillRightParen([H|T], Buff, Res, Rest, N):-
    N =\= 0, H = [H1|_], H1 == trightParen,
    append(Buff, [H], NewBuff), N1 is N - 1,
    packTillRightParen(T, NewBuff, Res, Rest, N1).
packTillRightParen([H|T], Buff, Res, Rest, N):-
    N =\= 0, H = [H1|_], \+ member(H1, [trightParen, tleftParen]),
    append(Buff, [H], NewBuff),
    packTillRightParen(T, NewBuff, Res, Rest, N).

%% Base
addNewLines([], [], []):- !.
% if ;  //  /**/, else
addNewLines([H|T], Buff, [NewBuff|R]):-
    H = [H1|_], member(H1, [tsemicolon, tcomment1, tcomment2, telse]),
    append(Buff, [H, [execNL, "\n"]], NewBuff), !,
    addNewLines(T, [], R).
%if }EOF
addNewLines([H|[]], Buff, [NewBuff|R]):-
    H = [H1|_], member(H1, [trightBrace]),
    append(Buff, [H, [execNL, "\n"]], NewBuff), !,
    addNewLines([], [], R).
% if {0,{;,{,,}0,};},
addNewLines([H1, H2|T], Buff, R):-
    H1 = [H11|_], member(H11, [tleftBrace, trightBrace]),
    H2 = [H21|_], member(H21, [tnumber, tsemicolon, tcomma]),
    append(Buff, [H1], NewBuff), !,
    addNewLines([H2|T], NewBuff, R).
%if not {0,{;,{,,}0,};},
addNewLines([H1, H2|T], Buff, [NewBuff|R]):-
    H1 = [H11|_], member(H11, [tleftBrace, trightBrace]),
    H2 = [H21|_], \+ member(H21, [tnumber, tsemicolon, tcomma]),
    append(Buff, [H1, [execNL, "\n"]], NewBuff), !,
    addNewLines([H2|T], [], R).
% if function, while, for, if, else
addNewLines([H1, H2|T], Buff, [NewBuff|R]):-
    H1 = [H11|_], member(H11, [tif, twhile, tfor]),
    packTillRightParen(T, [], MyPart, Rest, 1),
    append(Buff, [H1, H2], NewB1),
    append(NewB1, MyPart, NewBuff), !,
    addNewLines(Rest, [], R).
% if function void m(){...}
addNewLines([H, H2|T], Buff, [NewBuff|R]):-
    H = [H1|_], member(H1, [tfunction]),
    packTillRightParen(T, [], MyPart, Rest, 1),
    Rest \= [], Rest= [[M1|_]|_], M1 == tleftBrace,
    append(Buff, [H, H2], NewB1),
    append(NewB1, MyPart, NewBuff), !,
    addNewLines(Rest, [], R).
% if function like scanf("%d", &n);
addNewLines([H, H2|T], Buff, R):-
    H = [H1|_], member(H1, [tfunction]),
    packTillRightParen(T, [], _, Rest, 1),
    (Rest == [] ;  Rest= [[M1|_]|_], M1 \= tleftBrace),
    append(Buff, [H], NewBuff), !,
    addNewLines([H2|T], NewBuff, R).
% if #inclide
addNewLines([H1, H2|T], Buff, [NewBuff|R]):-
    H1 = [H11|_], member(H11, [tinclude]),
    append(Buff, [H1, H2, [execNL, "\n"]], NewBuff), !,
    addNewLines(T, [], R).
% if not any above just append them
addNewLines([H|T], Buff, R):-
    H = [H1|_],
    \+ member(H1, [tfunction, tsemicolon, tinclude, tif, telse, twhile,
                    tfor, tcomment1, tcomment2, tleftBrace, trightBrace]),
    append(Buff, [H], NewBuff), !,
    addNewLines(T, NewBuff, R).

%%Main predicate
addNewLines(Tokens, PackedLines):- addNewLines(Tokens, [], PackedLines), !.


%% Base
addSpaces([], Buff, Buff):- !.
%%  no spaces before and after '\n'
addSpaces([H|T], Buff, Res):-
    H = [H1|_], member(H1, [execNL]),
    ((append(NewBuf, [[tspace, " "]], Buff),
    append(NewBuf, [H], NewBuff));
    (\+ append(_, [[tspace, " "]], Buff),
    append(Buff, [H], NewBuff))), !,
    addSpaces(T, NewBuff, Res).
%% Before ',', ';' no space
addSpaces([H|T], Buff, Res):-
    H = [H1|_], member(H1, [tcomma, tsemicolon]),
    append(NewBuf, [[tspace, " "]], Buff),
    append(NewBuf, [H, [tspace, " "]], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%% No space after (, [, {, function name
addSpaces([H1|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tleftParen, tfunction, tleftBrace, tleftSqParen]),
    append(Buff, [H1], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%% i->mau();--
addSpaces([H1|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tptr, execNL]),
    append(NewBuf, [_], Buff),
    append(NewBuf, [H1], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%% some tokens before *i, &i
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tmultiply, tbitwiseAnd]),
    H2 = [H21|_], member(H21, [tidentifier]),
    length(Buff, N), N >= 2,
    append(_, [H3, H4], Buff),
    H3 = [H31|_], \+ member(H31, [tnumber, tidentifier, trightParen, trightSqParen]),
    H4 = [H41|_], member(H41, [tspace, tleftParen, tleftBrace, tleftSqParen]),
    append(Buff, [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%%  new line (*i, (&i
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tmultiply, tbitwiseAnd]),
    H2 = [H21|_], member(H21, [tidentifier]),
    length(Buff, 1),
    Buff = [[H31|_]], member(H31, [tleftParen, tleftBrace, tleftSqParen]),
    append(Buff, [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%% none before *i, &i
addSpaces([H1, H2|T], [], Res):-
    H1 = [H11|_], member(H11, [tmultiply, tbitwiseAnd]),
    H2 = [H21|_], member(H21, [tidentifier]),
    append([], [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%% i++, i--
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tidentifier]),
    H2 = [H21|_], member(H21, [tpp, tmm]),
    append(Buff, [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%% ++i, --i
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tpp, tmm]),
    H2 = [H21|_], member(H21, [tidentifier]),
    append(Buff, [H1], NewBuff), !,
    addSpaces([H2|T], NewBuff, Res).
%%  pack strings line "majhaaud830204ie1\n"
addSpaces([H1|T], Buff, Res):-
    H1 = [H11, Lex], member(H11, [tquot, tdoubleQuot]),
    append(M, [[H11,Lex]|Rest], T),
    append(Buff, [H1|M], NewBuf),
    append(NewBuf, [[H11,Lex]], NewBuff), !,
    addSpaces(Rest, NewBuff, Res).
%% })]]
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [trightParen, trightBrace, trightSqParen]),
    H2 = [H21|_], member(H21, [trightParen, trightBrace, trightSqParen]),
    append(Buff, [H1], NewBuff), !,
    addSpaces([H2|T], NewBuff, Res).
% 0], 0}, a], a}, 1), a), a][
addSpaces([H1, H2, H3|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tnumber, tidentifier]),
    H2 = [H21|_], member(H21, [trightParen, trightBrace, trightSqParen]),
    H3 = [H31|_],
    ((member(H31, [trightParen, trightBrace, trightSqParen, tleftSqParen]),
    append(Buff, [H1, H2], NewBuff));
    (\+ member(H31, [trightParen, trightBrace, trightSqParen]),
    append(Buff, [H1, H2, [tspace, " "]], NewBuff))), !,
    addSpaces([H3|T], NewBuff, Res).
% a[..
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tidentifier]),
    H2 = [H21|_], member(H21, [tleftSqParen]),
    append(Buff, [H1], NewBuff), !,
    addSpaces([H2|T], NewBuff, Res).
%%  Else
addSpaces([H|T], Buff, Res):-
    append(Buff, [H, [tspace, " "]], NewBuff), !,
    addSpaces(T, NewBuff, Res).

%% Main predicate
addSpaces([], []):- !.
addSpaces([HT|TT], Res):- addSpaces(HT, [], RHT), !,
    addSpaces(TT, RTT),
    append([RHT], RTT, Res).


%% Base
addTabs([], _, Buff, Buff):- !.
%% Openning tleftBrace
addTabs([H|T], Tabs, Buff, Res):-
    H = [[H1|_]|_],
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff),
    H1 == tleftBrace,
    append(Tabs, [[execTAB, "\t"]], NewTabs), !,
    addTabs(T, NewTabs, NewBuff, Res).
%% Closing trightBrace and a little newline for readability
addTabs([H|T], Tabs, Buff, Res):-
    H = [[H1|_]|_],
    H1 == trightBrace,
    append(NewTabs, [[execTAB, "\t"]], Tabs),
    append(NewTabs, H, NewH1),
    append(NewH1, [[execNL, "\n"]], NewH),
    append(Buff,[NewH], NewBuff), !,
    addTabs(T, NewTabs, NewBuff, Res).
%% Special case when one line only
addTabs([H|T], Tabs, Buff, Res):-
    H = [[H1|_]|_],
    member(H1, [tfor, twhile, tif, telse]),
    T = [[H2|HR]|TR],
    H2 = [H21|_],
    H21 \= tleftBrace,
    append(Tabs, H, NewH),
    append([[execTAB, "\t"]|Tabs], [H2|HR], AfterH),
    append(Buff,[NewH, AfterH], NewBuff), !,
    addTabs(TR, Tabs, NewBuff, Res).
%% Ordinary case
addTabs([H|T], Tabs, Buff, Res):-
    H = [[H1|_]|_],
    member(H1, [tfor, twhile, tif, telse]),
    T = [[[H2|_]|_]|_],
    H2 == tleftBrace,
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff), !,
    addTabs(T, Tabs, NewBuff, Res).
%% All other tokens
addTabs([H|T], Tabs, Buff, Res):-
    H = [[H1|_]|_],
    \+ member(H1, [tleftBrace, tfor, twhile, tif, telse, trightBrace]),
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff), !,
    addTabs(T, Tabs, NewBuff, Res).

%% Main predicate
addTabs(PackedLinesWithSpaces, PackedLinesWithSpacesAndTabs):-
    addTabs(PackedLinesWithSpaces, [], [], PackedLinesWithSpacesAndTabs), !.
