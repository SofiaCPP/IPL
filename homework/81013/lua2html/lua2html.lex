%{
const char* STRING = "str";
const char* COMMENT = "com";
const char* KEYWORD = "key";
const char* NUMBER = "num";
const char* IDENTIFIER = "id";
const char* OPERATOR = "op";
const char* OTHER = "oth";
const char* UNKNOWN = "unknwn";

void printToken(char* token, const char* type)
{
    printf("<span class=\"%s\">", type);
    while(*token)
    {
        switch(*token)
        {
            case '<':
            {
                printf("&lt;");
            }
            break;
            case '>':
            {
                printf("&gt;"); 
            }
            break;
            case '&':
            {
                printf("&amp;");
            }
            break;
            case '\"':
            {
                printf("&quot;");
            }
            break;
            case '\'':
            {
                printf("&apos;");
            }
            break;
            default:
            {
                printf("%c", *token);
            }
        }

        token++;
    }

    printf("</span>");
}

%}

DIGIT       [0-9]
IDENTIFIER  [a-zA-Z_][a-zA-Z0-9_]*

FLOAT_SIMPLE    {DIGIT}+"."{DIGIT}*
FLOAT_EXPONENT  {DIGIT}+"."{DIGIT}+([eE][+-]?){DIGIT}+

HEX 0x[a-fA-F0-9]+

%x IN_STRQ
%x IN_STRDQ
%x IN_MULTISTR
%x IN_COMMENT

%%

'   {
    printToken(yytext, STRING);
    BEGIN(IN_STRQ);
}
<IN_STRQ>\\\\   printToken(yytext, STRING);
<IN_STRQ>\\'    printToken(yytext, STRING);
<IN_STRQ>'  {
                printToken(yytext, STRING);
                BEGIN(INITIAL);
            }
<IN_STRQ>\n     printf("<br>");
<IN_STRQ>.      printToken(yytext, STRING);


\"  {
    printToken(yytext, STRING);
    BEGIN(IN_STRDQ);
}
<IN_STRDQ>\\\\  printToken(yytext, STRING);
<IN_STRDQ>\\\"  printToken(yytext, STRING);
<IN_STRDQ>\" {
                printToken(yytext, STRING);
                BEGIN(INITIAL);
            }
<IN_STRDQ>\n    printf("<br>");
<IN_STRDQ>.     printToken(yytext, STRING);


"[["    {
    printToken(yytext, STRING);
    BEGIN(IN_MULTISTR);
}
<IN_MULTISTR>"]]" {
                    printToken(yytext, STRING);
                    BEGIN(INITIAL);
                }
<IN_MULTISTR>\n       printf("<br>");
<IN_MULTISTR>.        printToken(yytext, STRING);

"--["=+"["    {
    printToken(yytext, COMMENT);
    BEGIN(IN_COMMENT);
}
<IN_COMMENT>"]"=+"]" {
                    printToken(yytext, COMMENT);
                    BEGIN(INITIAL);
                }
<IN_COMMENT>\n      printf("<br>");
<IN_COMMENT>.       printToken(yytext, COMMENT);


"--"[^[[\n]*\n    {
    printToken(yytext, COMMENT);
    printf("<br>");
}

{DIGIT}+ {
    printToken(yytext, NUMBER);
}

{HEX} {
    printToken(yytext, NUMBER);
}

{FLOAT_SIMPLE}      {
    printToken(yytext, NUMBER);
}
{FLOAT_EXPONENT}    {
    printToken(yytext, NUMBER);
}

and|break|do|else|elseif|end|false|for|function|if|in|local|nil|not|or|repeat|return|then|true|until|while {
    printToken(yytext, KEYWORD);
}

"+"|"-"|"*"|"/"|"%"|"^"|"#"|"=="|"~="|"<="|">="|"<"|">" {    
    printToken(yytext, OPERATOR);
}

"="|"("|")"|"{"|"}"|"["|"]"|";"|":"|","|"."|".."|"..."  {
    printToken(yytext, OTHER);
}

{IDENTIFIER} {
    printToken(yytext, IDENTIFIER);
}

" " {
    printf("&nbsp;");
}

\t  {
    printf("&nbsp;&nbsp;&nbsp;&nbsp;");
}

\n  {
    printf("<br>");
}

. {
    printToken(yytext, UNKNOWN);
}

%%

const char* STYLES = "<style>"
    ".com { color: #334b38; }"
    ".id { color: #cf512b; }"
    ".key { color: #731b1b; }"
    ".num { color: #275a5c; }"
    ".op { color: #080f0f; }"
    ".oth { color: #9c461e; }"
    ".str { color: #e49600; }"
    ".unknwn { color: #e102ff; }"
    "</style>";

int yywrap() { return 1; }

int main(int argc, char **argv)
{
    freopen("index.html", "w", stdout);

    printf("%s\n%s\n%s",
        "<!doctype>"
        "<html>"
        "<head>",
        STYLES,
        "</head>"
        "<body>"
        );

    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
    {
        yyin = fopen(argv[0], "r");
    }
    else
    {
        yyin = fopen("example.lua", "r");
    }

    yylex();

    printf("</body></html>");
    return 0;
}

