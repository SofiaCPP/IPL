/* scanner for a toy JavaScript-like language */

%{
#define YY_NO_UNISTD_H
%}

DIGIT    [0-9]
ID       [a-zA-Z_$][a-zA-Z0-9_$]*
STRQ     '([^\'\n]|(\\\'))*'
STRDQ    \"([^\"\n]|(\\\"))*\"

%%	

"/*"([^*]|[\n]|(\*+([^*/]|[\n])))*([*]+)"/" {
	printf("multi line comment %s\n", yytext);
	fprintf(yyout, "<span class=\"multi-line-comment\">%s</span>", yytext);
}

{DIGIT}+    {
			printf("Integer: %s\n", yytext);
            fprintf(yyout, "<span class=\"number\">%s</span>", yytext);
}

{DIGIT}*"."{DIGIT}*        {
			printf("Rational number: %s\n", yytext);
            fprintf(yyout, "<span class=\"number\">%s</span>", yytext);
}

"["((({DIGIT}+|{DIGIT}*"."{DIGIT}*)",")*|(({DIGIT}+|{DIGIT}*"."{DIGIT}*)",")*({DIGIT}+|{DIGIT}*"."{DIGIT}*))"]" {
	printf( "An array %s \n", yytext);
	fprintf(yyout, "<span class=\"array\">%s</span>", yytext);
}

"/""/"[^\n]* {
	printf("one line comment %s\n", yytext);
	fprintf(yyout, "<span class=\"one-line-comment\">%s</span>", yytext);
}




break|case|catch|class|const|continue|debugger|default|delete|do|else|export|extends|finally|for|function|if|import|in|instanceof|new|return|super|switch|this|throw|try|typeof|var|void|while|with|yield|private {
	printf( "Keyword: %s\n", yytext );
    fprintf(yyout, "<span class=\"keyword\">%s</span>", yytext);
}

{ID}   { 
	printf("Identifier: %s\n", yytext);
	fprintf(yyout, "<span class=\"identifier\">%s</span>", yytext);
}
{STRQ}|{STRDQ}        {
	printf("String: %s\n", yytext);
	fprintf(yyout, "<span class=\"string\">%s</span>", yytext);
}

"."|"+"|"-"|"*"|"/"|"="|"!"|">"|"<"|"!=="|"==="|">="|"<="|"++"|"--"|"=>" {
	printf("Operator: %s\n", yytext);
	fprintf(yyout, "<span class=\"operator\">%s</span>", yytext);
}

[;(){}:?]+   {
	printf("Skipped symbols: %s\n", yytext);
	fprintf(yyout, "%s", yytext); /* echo the rest */
}
.   {
	fprintf( yyout, "%s", yytext);
}

%%

int yywrap()
{
    return 1;
}


int main(int argc, const char* argv[])
{
    ++argv, --argc;  /* skip over program name */
/*    
	if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;
*/

	yyin = fopen("./hello.js", "r");



	yyout = fopen("./index.html", "w");

    fputs(
        "<!doctype>\n"
        "<html>\n"
        "<head>\n"
        "    <title>hello.js</title>\n"
        "    <style>\n"
        "        .keyword {\n"
        "            color: purple;\n"
        "        }\n"
        "        .number {\n"
        "            color: blue;\n"
        "        }\n"
        "        .string {\n"
        "            color: red;\n"
        "        }\n"
		"		.one-line-comment{\n"
		"			color:green		\n"					
		"		}\n"
		"		.multi-line-comment{\n"
		"			color:green		\n"					
		"		}\n"
        "        .operator {\n"
        "            font-style: bold;\n"
        "        }\n"
				
        "    </style>\n"
        "</head>\n"
        "<body>\n"
        "    <pre class=\"code\">\n", yyout
        );
    yylex();
    fputs("</pre></body></html>", yyout);
    return 0;
}
