%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
int yylex();
int yyerror(const char* error);
FILE *yyin;
#define YYDEBUG 1

int current_indent = 1;
int current_collapse_blocks = 0;

/**
 * Appends something to the first argument
 * @param char* where Where to append (Must be allocated memory or NULL).
 * @param char* what What to append to @param where
 */
char* append(char* where, char* what) {
    char * result;
    if (where == NULL)
    {
        result = malloc(strlen(what) + 1);
        sprintf(result, "%s", what);
    }
    else
    {
        result = realloc(where, strlen(where) + strlen(what) + 1);
        sprintf(result, "%s%s", where, what);
    }

    return result;
}

char* concat(char* first, char* second) {
    char* result = malloc(strlen(first) + strlen(second) + 1);
    sprintf(result, "%s%s", first, second);
    return result;
}

char* stylize(char* text, char* style) {
    char* result = malloc(23 + strlen(style) + strlen(text));
    sprintf(result, "<span class=\"%s\">%s</span>", style, text);
    return result;
}

char* openCollapsable(char * current) {
    append(current, "<button class=\"collapsible_btn active\">–</button>");
    append(current, "<div class=\"collapsable collapsable_dots\" style=\"display: none;\">...</div>");
    return append(current, "<div class=\"collapsable\" style=\"display: block;\">");
}

%}

%start input

%token KEYWORD
%token OPERATOR
%token STRING
%token MLSTRING
%token NUMBER
%token IDENTIFIER
%token LBRACKET
%token RBRACKET
%token COLON
%token NEWLINE
%token DBLNL
%token WHITESPACE 
%token COMMENT
%token NA
%union {
    char* val;
}

%type <val> input token_sequence token KEYWORD OPERATOR STRING MLSTRING NUMBER IDENTIFIER LBRACKET RBRACKET COLON NEWLINE DBLNL WHITESPACE COMMENT NA

%%

input: { }
    | token_sequence { printf("%s", $$); free($$); }
    ;

token_sequence: token_sequence token { $$ = concat($1, $2); }
        | token { $$ = append(NULL, $1); }
        ;

token: OPERATOR { $$ = stylize($1, "operator"); }
    | KEYWORD COLON NEWLINE {
        if (current_collapse_blocks > 0) {
            $$ = append(NULL, "</div>");
            $$ = append($$, stylize($1, "keyword")); 
        } else {
            $$ = append(stylize($1, "keyword"), $2); 
        }

        $$ = append($$, $3);
        $$ = openCollapsable($$);

        ++current_collapse_blocks;
    }
    | COLON NEWLINE { 
        $$ = append(stylize($1, "colon"), $2);
        $$ = openCollapsable($$);

        ++current_collapse_blocks;
    }
    | DBLNL { 
        if (current_collapse_blocks > 0) {
            --current_collapse_blocks;
            $$ = append(NULL, "</div>\n"); 
        } else {
            $$ = append(NULL, $1); 
        }
    }
    | KEYWORD { $$ = stylize($1, "keyword"); }
    | STRING { $$ = stylize($1, "string"); }
    | MLSTRING { $$ = stylize($1, "string"); }
    | NUMBER { $$ = stylize($1, "number"); }
    | IDENTIFIER { $$ = stylize($1, "identifier"); }
    | LBRACKET { $$ = stylize($1, "bracket"); }
    | RBRACKET { $$ = stylize($1, "bracket"); }
    | NEWLINE { $$ = stylize($1, "new_line"); }
    | WHITESPACE { $$ = stylize($1, "whitespace"); }
    | COLON { $$ = stylize($1, "colon"); }
    | COMMENT { $$ = stylize($1, "comment"); }
    | NA { $$ = append(NULL, $1); };

%%

int yyerror(const char* error)
{
    extern int current_line;
    fprintf(stderr, "input:%d: error: %s\n", current_line, error);
    return 0;
}

int main(int argc, const char* argv[])
{
    ++argv, --argc;
    if (argc > 0) {
        yyin = fopen(argv[0], "r");
    } else {
        yyin = stdin;
    }

    printf("<!DOCTYPE html>"
           "<html>\n"
           "<head>\n"
           "<meta charset=\"utf-8\">\n"
           "<title>Python script</title>\n"
           "<style>"
           ".code { background-color: #373737; padding: 1.5rem; }"
           ".string {color: blanchedalmond;}"
           ".whitespace { background-color: #0000000d; border-radius: 50%%; display: inline-block; height: 7px;width: 7px; overflow: hidden; }"
           ".keyword { color: greenyellow; }"
           ".operator { color: antiquewhite; }"
           ".number { color: slategray; }"
           ".bracket { color: violet; }"
           ".identifier { color: cornsilk; }"
           "div.collapsable {"
           "  margin-top: -1rem;"
           "  overflow: hidden;"
           "}"
           ".collapsable_dots {"
           "  margin-left: 1rem;"
           "  color: burlywood;"
           "}"
           "/* Style the button that is used to open and close the collapsible content */"
            ".collapsible_btn {"
            "  cursor: pointer;"
            "  font-size: .7rem;"
            "  margin: -2.3rem 0 0 -1.5rem;"
            "  position: absolute;"
            "}"
           "</style>\n"
           "</head>\n"
           "<body style=\"background: black;color: white;\">"
           "<pre class=\"code\">\n");

    yyparse();


    printf("</pre>\n"
           "</body>\n"

            "<script>"
            "var coll = document.getElementsByClassName(\"collapsible_btn\");\n"
            "var i;\n"
            "\n"
            "for (i = 0; i < coll.length; i++) {\n"
                "coll[i].addEventListener(\"click\", function() {\n"
                "this.classList.toggle(\"active\");\n"
                "var dots = this.nextElementSibling;\n"
                "if (dots.style.display === \"block\") {\n"
                    "dots.style.display = \"none\";\n"
                "} else {\n"
                    "dots.style.display = \"block\";\n"
                "}\n"
                "var content = this.nextElementSibling.nextElementSibling;\n"
                "if (content.style.display === \"block\") {\n"
                    "content.style.display = \"none\";\n"
                    "this.innerHTML=\"+\""
                "} else {\n"
                    "content.style.display = \"block\";\n"
                    "this.innerHTML=\"–\""
                "}\n"
                "});\n"
            "}\n"
            "</script>"

           "</html>\n");
    return 0;
}
