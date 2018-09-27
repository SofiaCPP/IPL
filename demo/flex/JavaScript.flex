/* scanner for a toy JavaScript-like language */

%{
/* need this for the call to atof() below */
#include <math.h>
%}

DIGIT    [0-9]
ID       [a-z_$][a-z0-9_$]*
STRQ     '([^\']|(\\\'))*'
STRDQ    "([^\"]|(\\\"))*"

%%

{DIGIT}+    {
            printf( "An integer: %s (%d)\n", yytext,
                    atoi( yytext ) );
            }

{DIGIT}+"."{DIGIT}*        {
            printf( "A float: %s (%g)\n", yytext,
                    atof( yytext ) );
            }

if|else|for|while|do|break|switch|case|function|return|new|delete|try|throw|catch|class|public|private {
    printf( "A keyword: %s\n", yytext );
}

{ID}        printf( "An identifier: %s\n", yytext );
{STRQ}|{STRDQ}        printf( "A string: %s\n", yytext );

"."|"+"|"-"|"*"|"/"|"="|"!"|">"|"<"|"!=="|"==="|">="|"<="|"++"|"--"|"=>"   printf( "An operator: %s\n", yytext );

";"   printf( "A semicolumn\n" );
"("   printf( "A left parenthesis\n" );
")"   printf( "A right parenthesis\n" );
"{"   printf( "A left bracket\n" );
"}"   printf( "A right bracket\n" );
":"   printf( "A colon\n" );
"?"   printf( "A question mark\n" );

[ \t\n]+          /* eat up whitespace */

.           printf( "Unrecognized character: %s\n", yytext );

%%

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

    /* YY_BUFFER_STATE hello = yy_scan_string("(x)=>{ return x !== 1; }"); */
    /* yypush_buffer_state(hello); */
    yylex();
    /* yy_delete_buffer(hello); */
    return 0;
}
