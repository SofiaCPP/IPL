%% Writing to html file the stream of tokens
tohtmlfile(_, []):- !.

tohtmlfile(Stream, [H|T]) :- is_identifier(H), H = [_,Lexem],
    write(Stream, '<span class=\"identifier\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_string(H), H = [_,Lexem],
    write(Stream, '<span class=\"string\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_function(H), H = [_, Lexem],
    write(Stream, '<span class=\"function\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_header(H), H = [_,Lexem],
    write(Stream, '<span class=\"string\">&lt;'),
    write(Stream, Lexem),
    write(Stream,'&gt;</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_keyWord(H), H = [_,Lexem],
    write(Stream, '<span class=\"keyword\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_number(H), H = [_,Lexem],
    write(Stream, '<span class=\"number\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_operator(H), H = [_,Lexem],
    write(Stream, '<span class=\"operator\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_parenth(H), H = [_,Lexem],
    write(Stream, '<span class=\"parenthese\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_quotation(H), H = [_,Lexem],
    write(Stream, '<span class=\"quotation\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_unknown(H), H = [_,Lexem],
    write(Stream, '<span class=\"unknown\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_comment(H), H = [_,Lexem],
    write(Stream, '<span class=\"comment\">'),
    write(Stream, Lexem),
    write(Stream,'</span>'),
    !,
    tohtmlfile(Stream, T).

tohtmlfile(Stream, [H|T]) :- is_controlStructureBody(H),
    write(Stream, '<button class=\"collapsible\">'),
    H = [H1|T1],
    tohtmlfile(Stream, H1),
    write(Stream, '</span></button>'), nl(Stream),
    write(Stream, '<div class=\"content\">'),
    helper(Stream, T1),
    write(Stream,'</div>'),
    !,
    tohtmlfile(Stream, T).


tohtmlfile(Stream, [H|T]) :- is_blank(H), H = [_,Lexem],
    write(Stream, Lexem),
    !,
    tohtmlfile(Stream, T).



helper(_, []).
helper(Stream, L):- append(A, [H|B], L), is_controlStructureBody(H),
    helper(Stream,A), tohtmlfile(Stream, [H]), helper(Stream,B).
helper(Stream, [H|T]):- tohtmlfile(Stream, H), helper(Stream, T).
