%% Definitions of tokens
token([tauto, "auto" ]) --> "auto", !.
token([tbreak, "break" ]) --> "break", !.
token([tcase, "case" ]) --> "case", !.
token([tchar, "char" ]) --> "char", !.
token([tconst, "const" ]) --> "const", !.
token([tcontinue, "continue" ]) --> "continue", !.
token([tdefault, "default" ]) --> "default", !.
token([tdo, "do" ]) --> "do", !.
token([tdouble, "double" ]) --> "double", !.
token([telse, "else" ]) --> "else", !.
token([tenum, "enum" ]) --> "enum", !.
token([textern, "extern" ]) --> "extern", !.
token([tfloat, "float" ]) --> "float", !.
token([tfor, "for" ]) --> "for", !.
token([tgoto, "goto" ]) --> "goto", !.
token([tif, "if" ]) --> "if", !.
token([tint, "int" ]) --> "int", !.
token([tlong, "long" ]) --> "long", !.
token([tregister, "register" ]) --> "register", !.
token([treturn, "return" ]) --> "return", !.
token([tshort, "short" ]) --> "short", !.
token([tsigned, "signed" ]) --> "signed", !.
token([tsizeof, "sizeof" ]) --> "sizeof", !.
token([tstatic, "static" ]) --> "static", !.
token([tstruct, "struct" ]) --> "struct", !.
token([tswitch,  "switch"]) --> "switch", !.
token([ttypedef, "typedef" ]) --> "typedef", !.
token([tunion, "union" ]) --> "union", !.
token([tunsigned, "unsigned" ]) --> "unsigned", !.
token([tvoid, "void" ]) --> "void", !.
token([tvolatile, "volatile" ]) --> "volatile", !.
token([twhile, "while" ]) --> "while", !.
token([tinclude, "#include" ]) --> "#include", !.

token([tcomment1, IA]) --> comment1(I), !,
    { atom_chars(IA,I)}.

%% Handling windows written files with "\r\n"
token([[tcomment2, IA], [execNL, '\n']]) --> comment2(I), !,
    {append(I1, [C1, C2], I),
    ((char_code('\r', C1), char_code('\n', C2), atom_chars(IA,I1));
    (\+ char_code('\r', C1), char_code('\n', C2),
    append(I1, [C1], I2), atom_chars(IA,I2)))}.

token([tstring, IA]) --> stringy(I), !,
    {atom_chars(IA,I)}.
token([theader, IA]) --> header(I), !,
    {append(IO, [_], I), append([_], II, IO), atom_chars(IA,II)}.
token([[tfunction, IA], [tleftParen, "("]]) --> func(I), !,
    {append(IO, [_], I), IO \= [], atom_chars(IA, IO)}.
token([tnumber, I]) --> number(I), !.

token([tbinShiftLeftAssign, "<<="]) --> "<<=", !.
token([tbinShiftRightAssign, ">>="]) --> ">>=", !.
token([tpp, "++"]) --> "++", !.
token([tmm, "--"]) --> "--", !.
token([tptr, "->"]) --> "->", !.
token([tgreaterEqual,">=" ]) --> ">=", !.
token([tequal, "=="]) --> "==", !.
token([tnotequal,"!=" ]) --> "!=", !.
token([tlessEqual, "<="]) --> "<=", !.
token([tplusAssign, "+="]) --> "+=", !.
token([tminusAssign, "-="]) --> "-=", !.
token([tmultAssign, "*="]) --> "*=", !.
token([tdivAssign, "/="]) --> "/=", !.
token([tlessEqual, "<="]) --> "<=", !.
token([tlogicalAnd, "&&"]) --> "&&", !.
token([tlogicalOr, "||"]) --> "||", !.
token([tbinShiftLeft, "<<"]) --> "<<", !.
token([tbinShiftRight, ">>"]) --> ">>", !.
token([tbinAndAssign, "&="]) --> "&=", !.
token([tbinXOrAssign, "^="]) --> "^=", !.
token([tbinOrAssign, "|="]) --> "|=", !.

token([tmultiply, "*"]) --> "*", !.
token([tdivide, "/"]) --> "/", !.
token([tmod, "%"]) --> "%", !.
token([tadd, "+"]) --> "+", !.
token([tsubtract, "-"]) --> "-", !.
token([tless, "<"]) --> "<", !.
token([tgreater, ">"]) --> ">", !.
token([tnot, "!"]) --> "!", !.
token([tassign,"="]) --> "=", !.
token([tbitwiseAnd, "&"]) --> "&", !.
token([tbitwiseOr, "|"]) --> "|", !.
token([tbitwiseXOr, "^"]) --> "^", !.
token([tbinCompliment, "~"]) --> "~", !.


token([tspace , "\\s"]) --> "\\s", !.
token([ttab , "\\t"]) --> "\\t", !.
token([tnl, "\\n"]) --> "\\n", !.
token([execSpace , "\s"]) --> "\s", !.
token([execTAB , "\s\s\s\s"]) --> "\t", !.
token([execNL, "\n"]) --> ( "\n"; "\r\n"), !.


token([tleftParen,"("]) --> "(", !.
token([trightParen, ")"]) --> ")", !.
token([tleftBrace, "{"]) --> "{", !.
token([trightBrace, "}"]) --> "}", !.
token([tleftSqParen, "["]) --> "[", !.
token([trightSqParen, "]" ]) --> "]", !.
token([tsemicolon, ";"]) --> ";", !.
token([ttwodots, ":"]) --> ":", !.
token([tquestion, "?"]) --> "?", !.

token([tcomma, "," ]) --> ",", !.
token([tquot, "\'"]) --> "\'", !.
token([tdoubleQuot, "\""]) --> "\"", !.
token([tdot, "."]) --> ".", !.

token([tidentifier, IA]) -->  identifier(I), !,
    {atom_chars(IA,I)}.

token([tunknown, IA]) --> allThatIsNotAToken(I), !,
    {I \= [], atom_chars(IA, I)}.


%% Main predicate
tokens([Token | Tail]) --> token(Token), !, tokens(Tail).
tokens([]) --> !, [], !.


%% Helper predicates
identifier([C|Cs]) --> [C], {char_type(C, csymf)}, !,
    identifierHelper(Cs).

identifierHelper([C|Cs]) --> [C], {char_type(C, csym)}, !,
    identifierHelper(Cs).
identifierHelper([C]) --> [C], {char_type(C, csym), \+ char_code('_', C)}, !.
identifierHelper([]) --> [], !.

func([C|Cs]) --> [C], {char_type(C, csymf)}, !,
    funkHelper(Cs).

funkHelper([C|Cs]) --> [C], {char_type(C, csym)}, !,
    funkHelper(Cs).
funkHelper([C]) --> [C], {char_code("(", C)}, !.

stringy([C|Cs]) --> [C], {char_code('\"', C)}, !,
    stringyHelper(Cs).

stringyHelper([C]) --> [C], { char_code('\n', C), fail}, !.
stringyHelper([C1, C2]) --> [C1, C2], { char_code('\\', C1), char_code('\s', C2), fail}, !.
stringyHelper([C1, C2|Cs]) --> [C1, C2], { char_code('\\', C1), char_code('\"', C2)}, !,
    stringyHelper(Cs).
stringyHelper([C]) --> [C], { char_code('\"', C)}.
stringyHelper([C|Cs]) --> [C], stringyHelper(Cs).

header([C|Cs]) --> [C], {char_code('<', C)}, !,
    headerHelper(Cs).

headerHelper([C|Cs]) --> [C],
    {char_type(C, csymf); char_code('.', C); char_code('/', C); char_code('+', C)},
    headerHelper(Cs).
headerHelper([C]) --> [C], {char_code('>', C)}.


comment1([C1,C2|Cs]) --> [C1,C2], {char_code('/', C1), char_code('*', C2)}, !,
    anythingButStarSlash(Cs).

comment2([C1,C2|Cs]) --> [C1,C2], {char_code('/', C1), char_code('/', C2)}, !,
    anythingButNL(Cs).

anything([C|Cs]) --> [C], anything(Cs).
anything([]) --> [].

allThatIsNotAToken([C|Cs]) --> [C|Cs], \+ tokens(C), \+ tokens([C|Cs]),
    allThatIsNotAToken(Cs).
allThatIsNotAToken([]) --> [].

anythingButNL([C|Cs]) --> [C],
    {\+ char_code('\r', C), \+ char_code('\n', C)},
    anythingButNL(Cs).
anythingButNL([C1,C2]) --> [C1,C2],
    {(char_code('\r', C1),  char_code('\n', C2));
    (\+ char_code('\r', C1),  char_code('\n', C2))}.

anythingButStarSlash([C1,C2]) --> [C1, C2],
    {char_code('*', C1),  char_code('/', C2)}.
anythingButStarSlash([C|Cs]) --> [C],
    anythingButStarSlash(Cs).



%% Definitions of lists of different types of tokens
is_keyWord([H,_]):- member(H, [tauto, tbreak, tcase, tchar, tconst, tcontinue,
    tdefault, tdo, tdouble, telse, tenum, textern, tfloat, tfor, tgoto, tif,
    tint, tlong, tregister, treturn, tshort, tsigned, tstatic, tstruct,
    tswitch, ttypedef, tunion, tunsigned, twhile, tvoid, tvolatile, tinclude]).

is_operator([H,_]):- member(H, [tmultiply, tdivide, tmod, tadd, tsubtract, tless,
    tlessEqual, tgreater, tgreaterEqual, tequal, tnotequal, tnot, tassign, tlogicalOr,
    tlogicalAnd, tbitwiseOr, tbitwiseAnd, tpp, tmm, tptr, tsizeof, tbinShiftLeftAssign,
    tbinShiftRightAssign, tplusAssign, tminusAssign, tmultAssign, tdivAssign, tbinShiftLeft,
    tbinShiftRight, tbinAndAssign, tbinXOrAssign, tbinOrAssign, tbitwiseXOr, tbinCompliment]).

is_parenth([H,_]):- member(H, [tleftParen, trightParen, tleftBrace, trightBrace,
    tleftSqParen, trightSqParen, tsemicolon, ttwodots]).

is_quotation([H,_]):- member(H, [tquot, tdoubleQuot, tcomma, tdot, tquestion]).

is_unknown([H,_]):- member(H, [tunknown]).

is_number([H,_]):- member(H, [tnumber]).

is_function([H,_]):- member(H, [tfunction]).

is_controlStructureBody([H|_]):-
    member(Token, [tdo, tfor, twhile, tif, telse, tswitch, tfunction, tstruct]),
    append(_, [[Token|_]|_], H).

is_packedFunction([H|_]):- append(_, [[tfunction|_]|_], H).

is_packedStructure([H|_]):- append(_, [[tstruct|_]|_], H).

% experimental is_functionWithCC([H|_]):- H = [[tCC]|_].

is_identifier([H,_]):- member(H, [tidentifier, tnl, ttab, tspace]).

is_blank([H,_]):- member(H, [execTAB, execSpace, execNL]).

is_comment([H,_]):-member(H, [tcomment1, tcomment2]).

is_string([H,_]):-  member(H, [tstring]).

is_header([H,_]):- member(H, [theader]).
