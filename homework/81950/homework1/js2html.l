
%{
#include<math.h>
#include <fcntl.h>
#include<stdio.h>
#include <stdlib.h>
#define YY_NO_UNISTD_H
%}

DIGIT    [0-9]
IDENTIFIER      [a-z_A-Z_$][a-z0-9A-Z_$]*
STRQ     '([^\']|(\\\'))*'
STRDQ    \"([^\"]|(\\\"))*\"
COMMENTSL  "//".*
COMMENTML  [/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]
BACKTICKS "`"

%%

{DIGIT}+    {
            printf("<span class=\"number\">%s</span>", yytext);
            }

{DIGIT}+"."{DIGIT}*        {
            printf("<span class=\"number\">%s</span>", yytext);
            }

function|new|delete|class|public|private|protected|const|debugger|default|enum|export|extends|false|implements|in|instanceof|interface|let|null|package|super|static|this|true|typeof|var|void {
    printf("<span class=\"keyword1\">%s</span>", yytext);
}

try|throw|catch|import|from|require|await|async|return|switch|case|default|if|else|for|while|do|break|continue|default|finally|with|yield {
    printf("<span class=\"keyword2\">%s</span>", yytext);
}

forEach|log|warn|error|info|assert|cos|sin|abs|acos|acosh|tan|trunc|cosh|sinh|keys|bind|call|apply|entries|freeze|UTC|now|isFinite|isNan|isInteger|isSafeInteger|resolve|reject|all {
    printf("<span class=\"method\">%s</span>", yytext);
}

Object|String|Date|Boolean|Number|Math|RegExp|Array|Promise|Map|File {
   printf("<span class=\"object\">%s</span>", yytext);
}

"exit" {
    return 0;  /* Type exit to quit the program */
}
{IDENTIFIER}        printf("<span class=\"identifier\">%s</span>", yytext);
{STRQ}      printf("<span class=\"string\">%s</span>", yytext);
{STRDQ}     printf("<span class=\"string\">%s</span>", yytext);
{COMMENTSL}   printf("<span class=\"comment\">%s</span>", yytext);
{COMMENTML}   printf("<span class=\"comment\">%s</span>", yytext);
{BACKTICKS}   printf("<span class=\"string\">%s</span>", yytext);

"."|"+"|"-"|"*"|"/"|"="|"%"|"!"|">>"|"<<"|">"|"<"|"!=="|"==="|">="|"<="|"++"|"--"|"=>"|"&"|"&&"|"|"|"||"   printf("<span class=\"operator\">%s</span>", yytext);

";"   printf("<span class=\"operator\">%s</span>", yytext);
","   printf("<span class=\"operator\">%s</span>", yytext);
"("   printf("<span class=\"left_parenthesis\">%s</span>", yytext);
")"   printf("<span class=\"right_parenthesis\">%s</span>", yytext);
"{"   printf("<span class=\"left_bracket\">%s</span>", yytext);
"}"   printf("<span class=\"right_bracket\">%s</span>", yytext);
":"   printf("<span class=\"colon\">%s</span>", yytext);
"?"   printf("<span class=\"question_mark\">%s</span>", yytext);
"["   printf("<span class=\"left_bracket\">%s</span>", yytext);
"]"   printf("<span class=\"right_bracket\">%s</span>", yytext);

"\n"  printf("<span>%s</span>",yytext);
.     printf("<span>%s</span>", yytext );
%%

int yywrap()
{
    return 1;
}


int main(int argc, const char* argv[])
{
    ++argv, --argc;
    if( argc > 0 ) {
        yyin = fopen(argv[0], "r");
    } else {
        printf("Enter Javascript code from the console: ( Type 'exit' to quit the program )\n");
        yyout = stdin;
    }
    int fd;
    char *name = "js2html.html";
    fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd == -1) {
        perror("open failed");
        exit(1);
    }
    if (dup2(fd, 1) == -1) {
        perror("dup2 failed"); 
        exit(1);
    }
    puts(
    "<!DOCTYPE html>"
    "<html>"
    "<head>"
        "<title>Javascript Highlighter</title>"
        "<style>"
            ".code {"
                "background-color: #000000;"
                "font-size: 25px;"
            "}"
            ".number {"
                "color: #90EE90;"
            "}"
            ".string {"
                "color: #ff9933;"
            "}"
            ".operator, .colon, .question_mark{"
                "color: #ffffff;"
            "}"
            ".keyword1, .left_bracket, .right_bracket {"
                "color: #0000bb;"
            "}"
            ".keyword2{"
                "color: #990099;"
            "}"
            ".comment{"
                "color: #009900;"
            "}"
            ".method{"
                "color: #ffff33;"
            "}"
            ".object {"
                "color: #00cc66;"
            "}"
            ".identifier {"
                "color: #0080ff;"
            "}"
            ".left_parenthesis, .right_parenthesis {"
                "color: #FFD700"
            "}"
        "</style>"
    "</head>"
    "<body class=\"code\">"
    "<pre>"
    );
    yylex();
    puts("</pre></body></html>");
    return 0;
}