%{
#include <iostream>
int yylex();
int yyerror(const char* error);
int current_line = 1;
%}

%code requires {#include <string>}
%start input
%define api.value.type {std::string}

%token KEYWORD
%token STRING
%token NUMBER
%token IDENTIFIER
%token OPERATOR
%token OTHER
%token NEWLINE
%token DECORATOR
%token COMMENT


%%

input:  %empty
        | program  { std::cout << "<html><head><style> \
                                    .keyword {color:blue;} \
                                    .operator {color:#0e7c31; font-weight: bold} \
                                    .number {color:#d119b2;} .string {color:red;} \
                                    .block {display: inline; overflow: hidden;} \
                                    .collapsible { background: none; border: 1px black solid;} \
                                    .comment {color:grey; font-style: italic;} \
                                    .line_number {opacity: 0.5;} \
                                    .decorator {color:#1a1f63; font-style: italic;} \
                                   </style> \
                                   </head><body><pre><span class=\"line_number\">1. </span>" << $1 << "</pre><script> \
                                    var block = document.getElementsByClassName(\"block\"); \
                                    var j; \
                                    for (j = 0; j < block.length; j++) { \
                                        block[j].style.display = \"inline\"; \
                                    } \
                                    var coll = document.getElementsByClassName(\"collapsible\"); \
                                    var i; \
                                    for (i = 0; i < coll.length; i++) { \
                                        coll[i].addEventListener(\"click\", function() { \
                                            var content = this.nextElementSibling; \
                                            if (content.style.display === \"inline\"){ \
                                                content.style.display = \"none\"; \
                                                this.innerHTML = \"+\"; \
                                            } else { \
                                                content.style.display = \"inline\"; \
                                                this.innerHTML = \"-\"; \
                                            } \
                                        }); \
                                    } \
                                   </script></body></html>"; }

program: expr
         | program expr {$$ = $1 + $2;}

expr:  block
       | nonblock

nonblock: KEYWORD { $$ = "<span class=\"keyword\">" + $1 + "</span>"; }
          | STRING { $$ = "<span class=\"string\">" + $1 + "</span>"; }
          | NUMBER { $$ = "<span class=\"number\">" + $1 + "</span>"; }
          | IDENTIFIER { $$ = "<span class=\"identifier\">" + $1 + "</span>"; }
          | OPERATOR { $$ = "<span class=\"operator\">" + $1 + "</span>"; }
          | NEWLINE { $$ = $1 + "<span class=\"line_number\">" + std::to_string(++current_line) + ". </span>";}
          | COMMENT { $$ = "<span class=\"comment\">" + $1 + "</span>"; }
          | DECORATOR { $$ = "<span class=\"decorator\">" + $1 + "</span>"; }
          | OTHER

block: '{' program '}' {$$ = "{" + std::string("<button class=\"collapsible\">-</button><div class=\"block\">") + $2 + "</div>}";}

%%
int yyerror(const char* error)
{
    std::cerr << "error:\n" << error;
    return 0;
}
int main()
{
    yyparse();
    return 0;
}
