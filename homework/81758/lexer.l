%{
#define YY_NO_UNISTD_H
#include <string.h>
%}

DIGIT	[0-9]
CHAR     '([^\']|(\\\'))*'
STR    \"([^\"]|(\\\"))*\"

%%

{DIGIT}+	    {fprintf(yyout,"<span class=\"number\">%s</span>", yytext); }
{DIGIT}+"."{DIGIT}* {fprintf(yyout,"<span class=\"number\">%s</span>", yytext); }
{CHAR} 		    {fprintf(yyout,"<span class=\"character\">%s</span>", yytext); }
{STR}		    {fprintf(yyout,"<span class=\"string\">%s</span>", yytext); }
"<"(.+)">" 	    {yytext++; int len = strlen(yytext); yytext[len-1] = '\0'; fprintf(yyout,"&lt;%s&gt;", yytext); }

auto|break|case|catch|class|char|const|continue|default|double|delete|do|else|enum|extern|explicit|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|nullptr|operator|private|protected|public|register|return|short|string|signed|sizeof|static|struct|switch|typedef|tempalte|this|thread|throw|true|try|typeid|typename|using|union|unsigned|virtual|void|volatile|while|alignas|alignof|bool|generic|static_assert|static_cast|thread_local|include {fprintf(yyout,"<span class=\"keyword\">%s</span>", yytext);}

"\\"|";"|":"|"?"|","|"."|"/"|"("|")"|"{"|"}"|"#"|"~"|"["|"]" {fprintf(yyout,"<span class=\"punctuator\">%s</span>", yytext);}

"|"|"::"|"->"|"++"|"--"|"+"|"-"|"="|"*"|"!"|"%"|"&"|"^"|"<<"|">>"|"<="|">="|"=="|"!="|"&&"|"||" 		    {fprintf(yyout,"<span class=\"operator\">%s</span>", yytext);}

\/\/.*\n {fprintf(yyout,"<span class=\"comment\">%s</span><br>", yytext);}


[\n]+          	    {fprintf(yyout,"<br>\n");}

.           {fprintf(yyout,"%s", yytext); } /* print the rest as is*/

%%

int yywrap()
{
    return 1;
}


int main(int argc, const char* argv[])
{

++argv, --argc;  /* skip over program name */

    if (argc > 0)
	{
        yyin = fopen(argv[0], "r");
	yyout = fopen("changed.html","w");
	}
    else
	{
        yyin = stdin;
	}


fputs("<!DOCTYPE html>\n"
	"<html>\n"
	"<head>\n"
	"<title> a site </title>\n"
	"<meta charset = utf-8>\n"
	"<style>\n"
	".keyword {color: aqua;}\n"
	".punctuator {color: HotPink;}\n"
	".number {color: red;}\n"
	".character {color: orange;}\n"
	".string {color: brown;}\n"
	".operator {color: Cyan;}\n"
	".comment {color: green;}\n"
	"</style>\n"
    	"</head>\n"
	"<body>\n"
	"<pre class=\"code\">", yyout);

yylex();

fputs("\n</pre></body>\n</html>",yyout);

if ( argc > 0 )
{
	fclose(yyin);
	fclose(yyout);
}

return 0;
}
