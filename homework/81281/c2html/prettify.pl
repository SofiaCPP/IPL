prettify(StreamTokens, PlainWithNLnSnT):-
    removeAllWhites(StreamTokens, Plain),
    % write(Plain),nl,write("Plain ------------"), nl,
    addNewLines(Plain, PlainWithNL),
    %  write(PlainWithNL),nl,write("PlainWithNL ------------"), nl,
    addSpaces(PlainWithNL, PlainWithNLnS),
    %  write(PlainWithNLnS),nl,write("PlainWithNLnS ------------"), nl,
    addTabs(PlainWithNLnS, PlainWithNLnSnT).
    % write(PlainWithNLnSnT),nl,write("PlainWithNLnSnT ------------"), nl.
    % write(PrettyStream).
    % write(PrettyStream), nl.

% Removes all white characters
removeAllWhites([], []):- !.
removeAllWhites([[H,_]|T], R):- member(H, [execTAB, execSpace, execNL, tunknown]),
    removeAllWhites(T, R), !.
removeAllWhites([[H, B]|T], [[H, B]|R]):- \+ member(H, [execTAB, execSpace, execNL, tunknown]),
    removeAllWhites(T, R), !.

% Pack Till ; for cases like char matrix[3][3] = {{'0', '0', '0'}, {'0', '1', '0'}, {'0', '1', '1'}};
packTillsemiColon([[tsemicolon, ";"]|T], Buff, Buff, [[tsemicolon, ";"]|T]).
packTillsemiColon([H|T], Buff, Res, Rest):-
    append(Buff, [H], NewBuff), !,
    packTillsemiColon(T, NewBuff, Res, Rest).

% Pack Till : and include them in result too. For case smt: where smt if more than 1 character
packTillTwoDots([[ttwodots, ":"]|T], Buff, NBuff, T):- append(Buff, [[ttwodots, ":"]], NBuff).
packTillTwoDots([H|T], Buff, Res, Rest):-
    append(Buff, [H], NewBuff), !,
    packTillTwoDots(T, NewBuff, Res, Rest).

% Pack Till ; for cases like char matrix[3][3] = {{'0', '0', '0'}, {'0', '1', '0'}, {'0', '1', '1'}};
packTillsemiColonOnlyIdentifiers([[tsemicolon, ";"]|T], Buff, Buff, [[tsemicolon, ";"]|T]).
packTillsemiColonOnlyIdentifiers([H1, H2|T], Buff, Res, Rest):-
    H1 = [H11|_], member(H11, [tcomma]),
    H2 = [H21|_], member(H21, [tidentifier]),
    append(Buff, [H1, H2], NewBuff), !,
    packTillsemiColonOnlyIdentifiers(T, NewBuff, Res, Rest).

% Pack for while, if, for and function arguments
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


% Base
addNewLines([], [], []):- !.
% if ;  //  /**/, do
addNewLines([H|T], Buff, [NewBuff|R]):-
    H = [H1|_], member(H1, [tsemicolon, tcomment1, tcomment2, tdo]),
    append(Buff, [H, [execNL, "\n"]], NewBuff), !,
    addNewLines(T, [], R).
% if else and no following if
addNewLines([H1, H2|T], Buff, [NewBuff|R]):-
    H1 = [H11|_], member(H11, [telse]),
    H2 = [H21|_], \+ member(H21, [tif]),
    append(Buff, [H1, [execNL, "\n"]], NewBuff), !,
    addNewLines([H2|T], [], R).
% if } while(..)
addNewLines([H1, H2|T], Buff, R):-
    H1 = [H11|_], member(H11, [trightBrace]),
    H2 = [H21|_], member(H21, [twhile]),
    packTillsemiColon(T, [], Res, Rest),
    append(Buff, [H1, H2], NewBuf),
    append(NewBuf, Res, NewBuff), !,
    addNewLines(Rest, NewBuff, R).
% % if typedef ...
% addNewLines([H1|T], Buff, R):-
%     H1 = [H11|_], member(H11, [ttypedef]),
%     packTillsemiColon(T, [], Res, Rest),
%     append(Buff, [H1], NewBuf),
%     append(NewBuf, Res, NewBuff), !,
%     addNewLines(Rest, NewBuff, R).
% if struct
addNewLines([H1, H2, H3|T], Buff, [NewBuff|R]):-
    H1 = [H11|_], member(H11, [tstruct]),
    H2 = [H21|_], member(H21, [tidentifier]),
    H3 = [H31|_], member(H31, [tleftBrace]),
    append(Buff, [H1, H2, [execNL, "\n"]], NewBuff), !,
    addNewLines([H3|T], [], R).
% if struct{ } id;
addNewLines([H1, H2|T], Buff, R):-
    H1 = [H11|_], member(H11, [trightBrace]),
    H2 = [H21|_], member(H21, [tidentifier]),
    packTillsemiColonOnlyIdentifiers(T, [], Res, Rest),
    append(Buff, [H1, H2], NewBuf),
    append(NewBuf, Res, NewBuff), !,
    addNewLines(Rest, NewBuff, R).
% if struct{ };
addNewLines([H1, H2|T], Buff, R):-
    H1 = [H11|_], member(H11, [trightBrace]),
    H2 = [H21|_], member(H21, [tsemicolon]),
    append(Buff, [H1, H2, [execNL, "\n"]], NewBuff), !,
    addNewLines(T, NewBuff, R).
% if char matrix[3][3] = {{'0', '0', '0'}, {'0', '1', '0'}, {'0', '1', '1'}};
addNewLines([H1, H2, H3|T], Buff, R):-
    H1 = [H11|_], member(H11, [tchar, tint, tdouble, tidentifier, tshort, tsigned, tunsigned, tenum, tfloat]),
    H2 = [H21|_], member(H21, [tidentifier]),
    H3 = [H31|_], member(H31, [tleftSqParen]),
    packTillsemiColon(T, [], Res, Rest),
    append(Buff, [H1, H2, H3], NewBuf),
    append(NewBuf, Res, NewBuff), !,
    addNewLines(Rest, NewBuff, R).
% if const char matrix[3][3] = {{'0', '0', '0'}, {'0', '1', '0'}, {'0', '1', '1'}};
addNewLines([H1, H2, H3, H4|T], Buff, R):-
    H1 = [H11|_], member(H11, [tconst]),
    H2 = [H21|_], member(H21, [tchar, tint, tdouble, tidentifier, tshort, tsigned, tunsigned, tenum, tfloat]),
    H3 = [H31|_], member(H31, [tidentifier]),
    H4 = [H41|_], member(H41, [tleftSqParen]),
    packTillsemiColon(T, [], Res, Rest),
    append(Buff, [H1, H2, H3, H4], NewBuf),
    append(NewBuf, Res, NewBuff), !,
    addNewLines(Rest, NewBuff, R).
% if const tenum {};
addNewLines([H1, H2, H3|T], Buff, R):-
    H1 = [H11|_], member(H11, [tconst]),
    H2 = [H21|_], member(H21, [tenum]),
    H3 = [H31|_], member(H31, [tidentifier]),
    packTillsemiColon(T, [], Res, Rest),
    append(Buff, [H1, H2, H3], NewBuf),
    append(NewBuf, Res, NewBuff), !,
    addNewLines(Rest, NewBuff, R).
% if const tenum {};
addNewLines([H1, H2|T], Buff, R):-
    H1 = [H11|_], member(H11, [tenum]),
    H2 = [H21|_], member(H21, [tidentifier]),
    packTillsemiColon(T, [], Res, Rest),
    append(Buff, [H1, H2], NewBuf),
    append(NewBuf, Res, NewBuff), !,
    addNewLines(Rest, NewBuff, R).
% if case smt:
addNewLines([H|T], Buff, [NewBuff|R]):-
    H = [H1|_], member(H1, [tcase]),
    packTillTwoDots([H|T], [], Res, Rest),
    append(Buff, Res, NewBuf),
    append(NewBuf, [[execNL, "\n"]], NewBuff), !,
    addNewLines(Rest, [], R).
% if default :
addNewLines([H|T], Buff, [NewBuff|R]):-
    H = [H1|_], member(H1, [tdefault]),
    T = [[ttwodots, ":"]|T1],
    append(Buff, [H, [ttwodots, ":"], [execNL, "\n"]], NewBuff), !,
    addNewLines(T1, [], R).
% if }EOF
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
% if not {0,{;,{,,}0,};},
addNewLines([H1, H2|T], Buff, [NewBuff|R]):-
    H1 = [H11|_], member(H11, [tleftBrace, trightBrace]),
    H2 = [H21|_], \+ member(H21, [tnumber, tsemicolon, tcomma]),
    append(Buff, [H1, [execNL, "\n"]], NewBuff), !,
    addNewLines([H2|T], [], R).
% if function, while, for, if
addNewLines([H1, H2|T], Buff, [NewBuff|R]):-
    H1 = [H11|_], member(H11, [tif, twhile, tfor, tswitch]),
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
    append(Buff, [H], NewBuff), !,
    addNewLines(T, NewBuff, R).

%%Main predicate
addNewLines(Tokens, PackedLines):- addNewLines(Tokens, [], PackedLines), !.


% Base
addSpaces([], Buff, Buff):- !.
%  no spaces before and after '\n' or ')'
addSpaces([H|T], Buff, Res):-
    H = [H1|_], member(H1, [execNL, trightParen]),
    ((append(NewBuf, [[execSpace, "\s"]], Buff),
    append(NewBuf, [H], NewBuff));
    (\+ append(_, [[execSpace, "\s"]], Buff),
    append(Buff, [H], NewBuff))), !,
    addSpaces(T, NewBuff, Res).
% Before ',', ';' no space
addSpaces([H|T], Buff, Res):-
    H = [H1|_], member(H1, [tcomma, tsemicolon]),
    append(NewBuf, [[execSpace, "\s"]], Buff),
    append(NewBuf, [H, [execSpace, "\s"]], NewBuff), !,
    addSpaces(T, NewBuff, Res).
% No space after (, [, {, function name, tsizeof
addSpaces([H1|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tleftParen, tfunction, tleftBrace, tleftSqParen,
    tsizeof]),
    append(Buff, [H1], NewBuff), !,
    addSpaces(T, NewBuff, Res).
% i->mau();--
addSpaces([H1|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tptr, execNL]),
    append(NewBuf, [_], Buff),
    append(NewBuf, [H1], NewBuff), !,
    addSpaces(T, NewBuff, Res).
% some tokens before *i, &i
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tmultiply, tbitwiseAnd]),
    H2 = [H21|_], member(H21, [tidentifier]),
    length(Buff, N), N >= 2,
    append(_, [H3, H4], Buff),
    H3 = [H31|_], \+ member(H31, [tnumber, tidentifier, trightParen, trightSqParen]),
    H4 = [H41|_], member(H41, [execSpace, tleftParen, tleftBrace, tleftSqParen]),
    append(Buff, [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
%  new line (*i, (&i
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tmultiply, tbitwiseAnd]),
    H2 = [H21|_], member(H21, [tidentifier]),
    length(Buff, 1),
    Buff = [[H31|_]], member(H31, [tleftParen, tleftBrace, tleftSqParen]),
    append(Buff, [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
% none before *i, &i
addSpaces([H1, H2|T], [], Res):-
    H1 = [H11|_], member(H11, [tmultiply, tbitwiseAnd]),
    H2 = [H21|_], member(H21, [tidentifier]),
    append([], [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
% i++, i--
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tidentifier]),
    H2 = [H21|_], member(H21, [tpp, tmm]),
    append(Buff, [H1, H2], NewBuff), !,
    addSpaces(T, NewBuff, Res).
% ++i, --i
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tpp, tmm]),
    H2 = [H21|_], member(H21, [tidentifier]),
    append(Buff, [H1], NewBuff), !,
    addSpaces([H2|T], NewBuff, Res).
%  pack strings line '0'
addSpaces([H1|T], Buff, Res):-
    H1 = [H11, Lex], member(H11, [tquot]),
    append(M, [[H11,Lex]|Rest], T),
    append(Buff, [H1|M], NewBuf),
    append(NewBuf, [[H11,Lex], [execSpace, "\s"]], NewBuff), !,
    addSpaces(Rest, NewBuff, Res).
% })]]
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
    append(Buff, [H1, H2, [execSpace, "\s"]], NewBuff))), !,
    addSpaces([H3|T], NewBuff, Res).
% a[..
addSpaces([H1, H2|T], Buff, Res):-
    H1 = [H11|_], member(H11, [tidentifier]),
    H2 = [H21|_], member(H21, [tleftSqParen]),
    append(Buff, [H1], NewBuff), !,
    addSpaces([H2|T], NewBuff, Res).
%  Else
addSpaces([H|T], Buff, Res):-
    append(Buff, [H, [execSpace, "\s"]], NewBuff), !,
    addSpaces(T, NewBuff, Res).

% Main predicate
addSpaces([], []):- !.
addSpaces([HT|TT], Res):- addSpaces(HT, [], RHT), !,
    addSpaces(TT, RTT),
    append([RHT], RTT, Res).


% Base
addTabs([], _, Buff, Buff, _, _):- !.
% Openning tleftBrace
addTabs([H|T], Tabs, Buff, Res, Bit1, Bit2):-
    H = [[H1|_]|_],
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff),
    H1 == tleftBrace,
    append(Tabs, [[execTAB, "\s\s\s\s"]], NewTabs), !,
    addTabs(T, NewTabs, NewBuff, Res, Bit1, Bit2).
% Closing trightBrace and a little newline for readability
addTabs([H|T], Tabs, Buff, Res, Bit1, Bit2):-
    H = [[H1|_]|_],
    H1 == trightBrace,
    append(NewTabs, [[execTAB, "\s\s\s\s"]], Tabs),
    append(NewTabs, H, NewH1),
    append(NewH1, [[execNL, "\n"]], NewH),
    append(Buff,[NewH], NewBuff), !,
    addTabs(T, NewTabs, NewBuff, Res, Bit1, Bit2).
% Special case when one line only and maybe nested ifs, fors, etc
addTabs([H|T], Tabs, Buff, Res, Bit1, Bit2):-
    H = [[H1|_]|_],
    member(H1, [tfor, twhile, tif, telse]),
    T = [[H2|_]|_],
    H2 = [H21|_],
    H21 \= tleftBrace,
    append(Tabs, H, NewH),
    append(Tabs, [[execTAB, "\s\s\s\s"]], NewTabs),
    append(Buff,[NewH], NewBuff), !,
    NewBit2 is Bit2 + 1,
    addTabs(T, NewTabs, NewBuff, Res, Bit1, NewBit2).
% Ordinary case
addTabs([H|T], Tabs, Buff, Res, Bit1, Bit2):-
    H = [[H1|_]|_],
    member(H1, [tfor, twhile, tif, telse, tswitch]),
    T = [[[H2|_]|_]|_],
    H2 == tleftBrace,
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff), !,
    addTabs(T, Tabs, NewBuff, Res, Bit1, Bit2).
%  case Bit1 helpes us to identify where if starts and ends
addTabs([H|T], Tabs, Buff, Res, 0, Bit2):-
    H = [[H1|_]|_],
    member(H1, [tcase, tdefault]),
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff),
    append(Tabs, [[execTAB, "\s\s\s\s"]], NewTabs), !,
    addTabs(T, NewTabs, NewBuff, Res, 1, Bit2).
% Ordinary case
addTabs([H|T], Tabs, Buff, Res, 1, Bit2):-
    H = [[H1|_]|_],
    member(H1, [tbreak]),
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff),
    append(NewTabs, [[execTAB, "\s\s\s\s"]], Tabs), !,
    addTabs(T, NewTabs, NewBuff, Res, 0, Bit2).
% All other tokens and Bit2 isn't 0
addTabs([H|T], Tabs, Buff, Res, Bit1, Bit2):-
    Bit2 \= 0,
    removeNTabs(Tabs, Bit2, NewTabs),
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff), !,
    addTabs(T, NewTabs, NewBuff, Res, Bit1, 0).
% All other tokens
addTabs([H|T], Tabs, Buff, Res, Bit1, Bit2):-
    append(Tabs, H, NewH),
    append(Buff,[NewH], NewBuff), !,
    addTabs(T, Tabs, NewBuff, Res, Bit1, Bit2).

% Main predicate
addTabs(PackedLinesWithSpaces, PackedLinesWithSpacesAndTabs):-
    addTabs(PackedLinesWithSpaces, [], [], PackedLinesWithSpacesAndTabs, 0, 0), !.
