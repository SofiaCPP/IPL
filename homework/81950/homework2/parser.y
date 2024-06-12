%{
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
int yylex();
int yyerror(const char* error);
extern int current_line_number;
#define YYDEBUG 0

void addStylesToTheHTML() {
    puts(
            "    <style>"
            "        .code {"
                        "background-color: #000000;"
                        "font-size: 20px;"
                        "padding: 10px 15px;"
                    "}"
            "        .identifier {"
            "            color: #0080ff;"
            "        }"
            "        .keyword1 {"
            "            color: #0000bb;"
            "        }"
            "        .keyword2 {"
            "            color: #990099;"
            "        }"
            "        .number {"
            "            color: #90EE90;"
            "        }"
            "        .string {"
            "            color: #ff9933;"
            "        }"
            "        .brace {"
            "           color: #990099;"
            "        }"
            "        .operator {"
            "            color: #ffffff;"
            "            font-weight: bold;"
            "        }"
            "        .comment{"
            "           color: #009900;"
            "         }"
            "        .method{"
                        "color: #ffff33;"
            "         }"
            "         .object {"
                        "color: #00cc66;"
            "         }"
            "        .unknown {"
            "            color: #ff0000;"
            "        }"
            "        .collapsible {"
            "            position: absolute;"
            "            left: 5px;"
            "            background-color: inherit;"
            "            color: white;"
            "            cursor: pointer;"
            "            padding: 2px;"
            "            border: none;"
            "            text-align: center;"
            "            outline: none;"
            "            font-size: 20px;"
            "        }"
            "     </style>"
    );
}

void addScriptForCollapsibleStructures() {
        puts(
                "<script>"
                "var collapsibleElement = document.getElementsByClassName(\"collapsible\");"
                "var i;"
                "for (i = 0; i < collapsibleElement.length; i++) {"
                  "collapsibleElement[i].addEventListener(\"click\", function() {"
                    "this.classList.toggle(\"active\");"
                    "var blockContent = this.nextElementSibling;"
                    "if (blockContent.style.display === \"block\") {"
                      "blockContent.style.display = \"none\";"
                      "this.innerHTML=\"〉\""
                    "} else {"
                      "blockContent.style.display = \"block\";"
                      "this.innerHTML=\"﹀\""
                    "}"
                  "});"
                "}"
                "</script>"
        );
}
char* twoStringsConcatenation(char* str1, char* str2) {
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    char* result;
    result = malloc(len1 + len2 + 1);
    strcpy(result, str1);
    strcat(result, str2);

    return result;
}


char* stylizeToken(char* token, char* tokenStyle) {
    char* result = twoStringsConcatenation("<span class=\"", tokenStyle);
    result = twoStringsConcatenation(result, "\">");
    result = twoStringsConcatenation(result, token);
    result = twoStringsConcatenation(result, "</span>");

    return result;
}

char* stylizeCollapsableBlock(char* collapsibleElement) {
        char* result = twoStringsConcatenation("<button class=\"collapsible active\">﹀</button>", "<div class=\"blockContent\" style=\"display: block\">");
        result = twoStringsConcatenation(result, collapsibleElement);
        result = twoStringsConcatenation(result, "</div>");
        return result;
}

%}

%start program


%token UNRECOGNIZED
%token NUMBER
%token KEYWORD1
%token KEYWORD2
%token OBJECT
%token METHOD
%token STRINGQ
%token STRINGDQ
%token OPERATOR
%token SPECIAL
%token IDENTIFIER
%token LBRACE
%token RBRACE
%token COMMENTSL
%token COMMENTML

%union {
    char* value;
}

%type <value> program expressions expr sequence_of_tokens token NUMBER KEYWORD1 KEYWORD2 OBJECT METHOD STRINGQ STRINGDQ IDENTIFIER OPERATOR SPECIAL UNRECOGNIZED LBRACE RBRACE COMMENTSL COMMENTML


%%

program:  /* empty */ { printf("empty\n"); }
    | expressions { printf("%s", $$); }
    ;

expressions: expr expressions { $$ = twoStringsConcatenation($1, $2); }
        | expr { $$ = $1; }
        ;

expr: sequence_of_tokens
        | LBRACE expressions RBRACE {
                        char* collapsableElement = stylizeCollapsableBlock($2);
                        $$ = twoStringsConcatenation("<span class=\"brace\">{</span>", collapsableElement);
                        $$ = twoStringsConcatenation($$, "<span class=\"brace\">}</span>");
                  }
        | LBRACE RBRACE { $$ = twoStringsConcatenation("<span class=\"brace\">{</span>", "<span class=\"brace\">}</span>"); }
        ;


sequence_of_tokens: sequence_of_tokens token { $$ = twoStringsConcatenation($1, $2); }
        | token { $$ = $1; }
        ;

token: NUMBER { $$ = stylizeToken($1, "number"); }
    | KEYWORD1 { $$ = stylizeToken($1, "keyword1"); }
    | KEYWORD2 { $$ = stylizeToken($1, "keyword2"); }
    | METHOD { $$ = stylizeToken($1, "method"); }
    | OBJECT { $$ = stylizeToken($1, "object"); }
    | OPERATOR { $$ = stylizeToken($1, "operator"); }
    | STRINGQ { $$ = stylizeToken($1, "string"); }
    | STRINGDQ { $$ = stylizeToken($1, "string"); }
    | IDENTIFIER { $$ = stylizeToken($1, "identifier"); }
    | SPECIAL { $$ = stylizeToken($1, "operator"); }
    | COMMENTSL { $$ = stylizeToken($1, "comment"); }
    | COMMENTML { $$ = stylizeToken($1, "comment"); }
    | UNRECOGNIZED { $$ = stylizeToken($1, "unknown"); }
    ;

%%

int yyerror(const char* error)
{
    fprintf(stderr, "input:%d: error: %s\n", current_line_number, error);
    return 0;
}


void printToFileHtmlHeader() {
       puts(
                   "<head>"
                   "    <title>example.js</title>"
       );
       addStylesToTheHTML();
       puts(
        "</head>"
       );
}



void printToFileBody() {
    puts(
      "<body class=\"code\">"
      "    <pre class=\"code\">"
      );
      yyparse();
      puts("</pre></body>");
}

void printToFileHTML() {
    puts(
      "<!doctype>"
      "<html>"
    );
    printToFileHtmlHeader();
    printToFileBody();
    addScriptForCollapsibleStructures();
    puts("</html>");
}


int main(int argc, const char* argv[])
{
    printToFileHTML();
    return 0;
}