%{
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
int yylex();
int yyerror(const char* error);
#define YYDEBUG 1

char* concatenate(char* first, char* second) {
    int firstlen = strlen(first);
    int secondlen = strlen(second);
    char* result;
    result = malloc(firstlen + secondlen + 1);
    strcpy(result, first);
    strcat(result, second);

    return result;
}

void printStyles() {
    puts(
            "    <style>"
            "        .identifier {"
            "            color: brown;"
            "        }"
            "        .keyword {"
            "            color: #1fbfe2;"
            "        }"
            "        .number {"
            "            color: blue;"
            "        }"
            "        .string {"
            "            color: green;"
            "        }"
            "        .operator {"
            "            color: #db184c;"
            "            font-style: bold;"
            "        }"
            "        .collapsible {"
            "            background-color: #777;"
            "            color: white;"
            "            cursor: pointer;"
            "            padding: 2px;"
            "            width: 2%;"
            "            border: none;"
            "            text-align: center;"
            "            outline: none;"
            "            font-size: 15px;"
            "        }"

            "        .active, .collapsible:hover {"
            "            background-color: #555;"
            "        }"


            "     </style>"
    );
}

void printCollapsableScript() {
        puts(
                "<script>"
                "var coll = document.getElementsByClassName(\"collapsible\");\n"
                "var i;\n"
                "\n"
                "for (i = 0; i < coll.length; i++) {\n"
                  "coll[i].addEventListener(\"click\", function() {\n"
                    "this.classList.toggle(\"active\");\n"
                    "var content = this.nextElementSibling;\n"
                    "if (content.style.display === \"block\") {\n"
                      "content.style.display = \"none\";\n"
                      "this.innerHTML=\"-\""
                    "} else {\n"
                      "content.style.display = \"block\";\n"
                      "this.innerHTML=\"+\""
                    "}\n"
                  "});\n"
                "}\n"
                "</script>"
        );
}

char* stylize(char* text, char* style) {
    char* result = concatenate("<span class=\"", style);
    result = concatenate(result, "\">");
    result = concatenate(result, text);
    result = concatenate(result, "</span>");

    return result;
}

char* collapsableBlock(char* text) {
        char* result = concatenate("<button class=\"collapsible\">+</button>", "<div class=\"content\">");
        result = concatenate(result, text);
        result = concatenate(result, "</div>");

        return result;
}

void printNumber(char* text) {
   char* style = stylize(text, "number");
   printf("%s", style);
}


void printIdentifier(char* text) {
   char* style = stylize(text, "identifier");
   printf("%s", style);
}

void printKeyword(char* text) {
   char* style = stylize(text, "keyword");
   printf("%s", style);
}

void printString(char* text) {
   char* style = stylize(text, "string");
   printf("%s", style);
}

void printOperator(char* text) {
     char* style = stylize(text, "operator");
     printf("%s", style);
}

void printMisc(char* text) {
    printf("%s", text);
}

void printUnrecognized(char* text) {
    printf("Unrecognized token: %s", text);
}

void printBraces(char* text) {
    printf("%s", text);
}

%}

%start input


%token NUMBER
%token KEYWORD
%token STRING
%token STRING_DQ
%token IDENTIFIER
%token OPERATOR
%token MISC
%token UNRECOGNIZED
%token LCURLY
%token RCURLY

%union {
    char* strval;
}

%type <strval> input expr_list expr token_sequence token NUMBER KEYWORD STRING STRING_DQ IDENTIFIER OPERATOR MISC UNRECOGNIZED LCURLY RCURLY


%%

input:  /* empty */ { printf("empty\n"); }
    | expr_list { printf("%s", $$); }
    ;

expr_list: expr expr_list { $$ = concatenate($1, $2); }
        | expr { $$ = $1; }
        ;

expr: token_sequence
        | LCURLY expr_list RCURLY {
                        char* collapsable = collapsableBlock($2);
                        $$ = concatenate("{", collapsable);
                        $$ = concatenate($$, "}");
                  }
        | LCURLY RCURLY { $$ = concatenate("{", "}"); }
        ;


token_sequence: token_sequence token { $$ = concatenate($1, $2); }
        | token { $$ = $1; }
        ;

token: NUMBER { $$ = stylize($1, "number"); }
    | KEYWORD { $$ = stylize($1, "keyword"); }
    | STRING { $$ = stylize($1, "string"); }
    | STRING_DQ { $$ = stylize($1, "string"); }
    | IDENTIFIER { $$ = stylize($1, "identifier"); }
    | OPERATOR { $$ = stylize($1, "operator"); }
    | MISC { $$ = $1; }
    | UNRECOGNIZED { $$ = $1 }
    ;

%%

int yyerror(const char* error)
{
    extern int current_line;
    fprintf(stderr, "input:%d: error: %s\n", current_line, error);
    return 0;
}


void printHtmlHeader() {
       puts(
                   "<head>"
                   "    <title>hello_world.go</title>"
       );
       printStyles();
       puts(
        "</head>"
       );
}



void printBody() {
    puts(
      "<body bgcolor=\"#e2cdb7\">"
      "    <pre class=\"code\">"
      );
      yyparse();
      puts("</pre></body>");
}

void printHTML() {
    puts(
      "<!doctype>"
      "<html>"
    );
    printHtmlHeader();
    printBody();
    printCollapsableScript();
    puts("</html>");
}


int main(int argc, const char* argv[])
{
    printHTML();
    return 0;
}
