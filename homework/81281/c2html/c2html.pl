:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).
:- [tokens, prettify, print, essentials, controlStructures].
:- set_prolog_flag(verbose, silent).

:- initialization(main, main).

main :-
    current_prolog_flag(argv, Argv),
    Argv = [H1, H2|_],
    atom_string(H1, File),
    atom_string(H2, Number),
    mainy(File, Number),
    format('You can see the result in the html file.~n'),
    halt.
main :-
    halt(1).

mainy(File, Number):-
    open(File, read, RFile),
    read_stream_to_codes(RFile, CSource),
    close(RFile),
    phrase(tokens(Tokens), CSource),
    flattenMine(Tokens, TokenStream),
    ((Number == "0", PrettyCode = TokenStream);
    (Number == "1", prettify(TokenStream, Raw),
     flattenMine(Raw, PrettyCode));
    (Number == "2", prettify(TokenStream, Raw),
     identifyControlStructures(Raw, RawF),
     flattenMine(RawF, PrettyCode))),
    open('page.html', write, WFile),
     write(WFile,'<!doctype>
    <html>

        <head>
            <title>hello.c</title>
            <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
            <style>
                .keyword {
                    color: blue;
                }

                .number {
                    color: violet;
                }

                .identifier {
                    color: green;
                }

                .operator {
                    font-style: bold;
                    color: blue;
                }

                .comment {
                    color: lime;
                }

                .string {
                    color: orange;
                }

                .parenthese {
                    font-style: bold;
                    color: brown;
                }

                .quotation {
                    font-style: bold;
                    color: black;
                }

                .unknown {
                    font-style: bold;
                    color: red;
                }

                .function {
                    color: purple;
                }

                .collapsible {
                    background-color: PaleGreen;
                    cursor: pointer;
                    padding: 10px;
                    width: 100%;
                    border: none;
                    text-align: left;
                    outline: none;
                    font-size: 15px;
                }

                .active,
                .collapsible:hover {
                    background-color: PaleGoldenRod;
                }

                .content {
                    padding: 0 18px;
                    display: none;
                    overflow: hidden;
                    background-color: #f1f1f1;
                }
            </style>
        </head>

        <body>
            <pre class=\"code\">'),
    tohtmlfile(WFile, PrettyCode),
    write(WFile, ' <script>
                    var coll = document.getElementsByClassName(\"collapsible\");
                    var i;

                    for (i = 0; i < coll.length; i++) {
                      coll[i].addEventListener(\"click\", function() {
                        this.classList.toggle(\"active\");
                        var content = this.nextElementSibling;
                        if (content.style.display === \"block\") {
                          content.style.display = \"none\";
                        } else {
                          content.style.display = \"block\";
                        }
                      });
                    }
                    </script>
                        </pre>
                    </body>

                    </html>'),
    close(WFile).
