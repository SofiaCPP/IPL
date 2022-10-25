%{
#include <string.h>
#define YY_NO_UNISTD_H

const char* basicWrapper(const char* class);

const char* functionWrapper(const char* class);
%}

RESERVED    as|break|class|construct|continue|else|false|for|foreign|if|import|in|is|null|return|static|super|this|true|var|while

DIGIT       [0-9]
HEX_LETTER  [a-f]|[A-F]
BASIC_ID    [a-z][A-Za-z0-9_]*
FIELD_ID    [_][A-Za-z0-9_]*
CLASS_ID    [A-Z][A-Za-z0-9_]*
TRPL_QUOTE  \"{3} 
/* regex magic to match anything but triple quotes inside triple quotes */
STRRAW      {TRPL_QUOTE}([^\"]|\"([^\"]|\"[^\"]))*{TRPL_QUOTE}
STRQ        '([^\'\n]|(\\\'))*'
STRDQ       \"([^\"\n]|(\\\"))*\"

WHITESPACE  [ \t]
NEWLINE     [\n]
COMMENT     \/\*.*\*\/|\/\/.*

HEX_NUMBER  0x({DIGIT}|{HEX_LETTER})+
NUMBER      -?{DIGIT}*(\.{DIGIT}+)?(e[-+]?[0-9]*)?|{HEX_NUMBER}

%%

{RESERVED} {
    basicWrapper("reserved");
}

{NUMBER}    basicWrapper("number");

{BASIC_ID}        basicWrapper("identificator");
{CLASS_ID}  basicWrapper("class");
{FIELD_ID}  basicWrapper("field");

{RESERVED}{WHITESPACE}*[(]  functionWrapper("reserved");
{RESERVED}{WHITESPACE}*[{]  functionWrapper("reserved");

{BASIC_ID}{WHITESPACE}*[(]  functionWrapper("function");
{BASIC_ID}{WHITESPACE}*[{]  functionWrapper("function");

{STRRAW}|{STRQ}|{STRDQ}        basicWrapper("string");

"."   basicWrapper("dot");

"-"|"!"|"~"|"*"|"/"|"%"|"+"|"-"|"<"|">"|"&"|"^"|"|"|"=" basicWrapper("operator");

";"|"("|")"|"["|"]"|"{"|"}"|","|":"|"?"                 printf( "%s", yytext );


{NEWLINE}       printf("<br>");
{WHITESPACE}    printf( "&nbsp;");
{COMMENT}       basicWrapper("comment");

.           printf( "\nUNRECOGNISED: \"%s\"\n", yytext );

%%

const char* basicWrapper(const char* class)
{
     printf("<span class='%s'>%s</span>", class, yytext);
}

const char* functionWrapper(const char* class)
{
    const char symbol =  yytext[strlen(yytext) - 1];
    yytext[strlen(yytext) - 1] = '\0';

    basicWrapper(class);

    printf("%c", symbol);
}

int yywrap()
{
    return 1;
}


int main(int argc, const char* argv[])
{
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;

    printf(
        "<!DOCTYPE html><html><head>"
        "<style>"
            "@charset \"utf-8\";"
            "body{background-color: darkslategrey; color: lavender; font-family: monospace; font-size: larger;}"
            "span{white-space:pre-wrap;}"
            ".reserved{color: tomato;}"
            ".class{color: aquamarine;}"
            ".number{color: lightskyblue;}"
            ".string{color: khaki;}"
            ".function{color: #F190FF;}"
            ".operator{color: darkorange;}"
            ".dot{color: turquoise;}"
            ".comment{color:darkseagreen;}"
        "</style>"
        "</head><body>"
    );

    yylex();
    
    printf("</body></html>");

    return 0;
}
