%{
#include <cstring>
#include "scheme2html.hpp"
#define WHITESPACE 1000
#define COMMENT 2000
#define KEYWORD 3000
#define NUMBER 4000
#define IDENTIFIER 5000
#define UNKNOWN 6000
#define TRUE 7000
#define FALSE 8000
%}
TRUE                (#t)
FALSE               (#f)
WHITESPACE          (" "|"\n")
COMMENT             ;.*
STRING              "([^"\\]|\\"|\\\\)*"
CHAR                #\\.*|#\\space|#\\newline
DIGIT               [0-9]*
INITIAL             ([a-z]|[A-Z])*|"!"|"\$"|"\%"|"\verb"&""|"*"|"/"|":"|"<"|"="|">"|"?"|"\verb\" \""|"\verb\"_\""|"\verb\"^\""
SPECIAL_SUBSEQUENT  .|\+|-
SUBSEQUENT          ({INITIAL})|({DIGIT})
PECULIAR_IDENTIFIER \+|-|...
IDENTIFIER          ({INITIAL})({SUBSEQUENT})*
KEYWORD             quote|lambda|if|set!|begin|cond|and|or|case|let|let!|letrec|do|delay|quasiquote|else|=>|define|unquote|unquote-splicing
EXACTNESS           (#i|#e)?
SIGN                (\+|-)?
RADIX_BINARY        (#b)
PREFIX_BINARY       ({RADIX_BINARY})({EXACTNESS})|({EXACTNESS})({RADIX_BINARY})
DIGIT_BINARY        (0|1)
UNINTEGER_BINARY    ({DIGIT_BINARY})+(#)*
UREAL_BINARY        ({UNINTEGER_BINARY})|({UNINTEGER_BINARY})\/({UNINTEGER_BINARY})
REAL_BINARY         ({SIGN})({UREAL_BINARY})
COMPLEX_BINARY      ({REAL_BINARY})|({REAL_BINARY})\+({UREAL_BINARY})i|({REAL_BINARY})-({UREAL_BINARY})i|({REAL_BINARY})\+i|({REAL_BINARY})-i|\+({UREAL_BINARY})i|-({UREAL_BINARY})i|\+i|-i
NUMBER_BINARY       ({PREFIX_BINARY})({COMPLEX_BINARY})
RADIX_OCTAL         (#o)
PREFIX_OCTAL        ({RADIX_OCTAL})({EXACTNESS})|({EXACTNESS})({RADIX_OCTAL})
DIGIT_OCTAL         (0|1|2|3|4|5|6|7)
UNINTEGER_OCTAL     ({DIGIT_OCTAL})+(#)*
UREAL_OCTAL         ({UNINTEGER_OCTAL})|({UNINTEGER_OCTAL})\/({UNINTEGER_OCTAL})
REAL_OCTAL          ({SIGN})({UREAL_OCTAL})
COMPLEX_OCTAL       ({REAL_OCTAL})|({REAL_OCTAL})\+({UREAL_OCTAL})i|({REAL_OCTAL})-({UREAL_OCTAL})i|({REAL_OCTAL})\+i|({REAL_OCTAL})-i|\+({UREAL_OCTAL})i|-({UREAL_OCTAL})i|\+i|-i
NUMBER_OCTAL        ({PREFIX_OCTAL})({COMPLEX_OCTAL})
RADIX_HEX           (#x)
PREFIX_HEX          ({RADIX_HEX})({EXACTNESS})|({EXACTNESS})({RADIX_HEX})
DIGIT_HEX           (0|1|2|3|4|5|6|7|8|9|a|b|c|d|e|f)
UNINTEGER_HEX       ({DIGIT_HEX})+(#)*
UREAL_HEX           ({UNINTEGER_HEX})|({UNINTEGER_HEX})\/({UNINTEGER_HEX})
REAL_HEX            ({SIGN})({UREAL_HEX})
COMPLEX_HEX         ({REAL_HEX})|({REAL_HEX})\+({UREAL_HEX})i|({REAL_HEX})-({UREAL_HEX})i|({REAL_HEX})\+i|({REAL_HEX})-i|\+({UREAL_HEX})i|-({UREAL_HEX})i|\+i|-i
NUMBER_HEX          ({PREFIX_HEX})({COMPLEX_HEX})
RADIX_DECIMAL       (#d)?
PREFIX_DECIMAL      ({RADIX_DECIMAL})({EXACTNESS})|({EXACTNESS})({RADIX_DECIMAL})
DIGIT_DECIMAL       (0|1|2|3|4|5|6|7|8|9)
UNINTEGER_DECIMAL   ({DIGIT_DECIMAL})+(#)*
EXPONENT_MARKER     (e|s|f|d|i)
SUFFIX              (({SIGN})(DIGIT_DECIMAL)+(EXPONENT_MARKER))?
DECIMAL             ({UNINTEGER_DECIMAL})({SUFFIX})|\.(DIGIT_DECIMAL)+(#)*({SUFFIX})|({DIGIT_DECIMAL})+\.({DIGIT_DECIMAL})*(#)*({SUFFIX})|({DIGIT_DECIMAL})+(#)+.(#)*({SUFFIX})
UREAL_DECIMAL       ({UNINTEGER_DECIMAL})|({UNINTEGER_DECIMAL})\/({UNINTEGER_DECIMAL})|({DECIMAL})
REAL_DECIMAL        ({SIGN})({UREAL_DECIMAL})
COMPLEX_DECIMAL     ({REAL_DECIMAL})|({REAL_DECIMAL})\+({UREAL_DECIMAL})i|({REAL_DECIMAL})-({UREAL_DECIMAL})i|({REAL_DECIMAL})\+i|({REAL_DECIMAL})-i|\+({UREAL_DECIMAL})i|-({UREAL_DECIMAL})i|\+i|-i
NUMBER_DECIMAL      ({PREFIX_DECIMAL})({COMPLEX_DECIMAL})
NUMBER              ({NUMBER_BINARY})|({NUMBER_OCTAL})|({NUMBER_HEX})|({NUMBER_DECIMAL})
UNKNOWN             (.)
%%
{FALSE}             {return FALSE;}
{TRUE}              {return TRUE;}
{COMMENT}           {return COMMENT;}
{KEYWORD}           {return KEYWORD;}
{NUMBER}            {return NUMBER;}
{WHITESPACE}        {return WHITESPACE;}
{IDENTIFIER}        {return IDENTIFIER;}
"("                 {return '(';}
")"                 {return ')';}
{UNKNOWN}           {return UNKNOWN;}
%%

int yywrap()
{
    return 1;
}

int main(int argc, char* argv[])
{
    ++argv, --argc;
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;
    int tokenType;
    FILE* out = fopen("output.html", "w");
    fputs("<!DOCTYPE html>\n<html>\n<head>\n", out);
    fputs("<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js\"></script>\n", out);
    fputs("<script src=\"./click-service.js\"></script>", out);
    fputs("</head><body style=\"background-color:#1e1e1e\">", out);
    while(tokenType = yylex())
    {
        put_token(tokenType, yytext);
    }
    print(out);
    fputs("</body>\n</html>", out);
    fclose(out);
}