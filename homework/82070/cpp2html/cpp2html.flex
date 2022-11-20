/* scanner for a C++ language */

%{
#define YY_NO_UNISTD_H
%}

DIGIT    [0-9]
ID       [a-zA-Z_$][a-zA-Z0-9_$]*
CHARSEQ  '([^\']|(\\\'))*'
STR      \"([^\"]|(\\\"))*\"
COMMENT  \/\/.*\n|\/\*.*\*\/

%%

{DIGIT}+    {
            printf("<span class=\"number\">%s</span>", yytext);
            }

{DIGIT}+"."{DIGIT}*        {
            printf("<span class=\"number\">%s</span>", yytext);
            }

alignas|alignof|and|and_eq|asm|atomic_cancel|atomic_commit|atomic_noexcept|auto|bitand|bitor|bool|break|case|catch|char|char8_t|char16_t|char32_t|class|compl|concept|const|consteval|constexpr|constinit|const_cast|continue|co_await|co_return|co_yield|decltype|default|delete|do|double|dynamic_cast|else|enum|explicit|export|extern|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|noexcept|not|not_eq|nullptr|operator|or|or_eq|private|protected|public|reflexpr|register|reinterpret_cast|requires|return|short|signed|sizeof|static|static_assert|static_cast|struct|switch|synchronized|template|this|thread_local|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile|wchar_t|while|xor|xor_eq {
            printf("<span class=\"keyword\">%s</span>", yytext);
            }

#if|#elif|#else|#endif|#ifdef|#ifndef|#elifdef|#elifndef|#define|#undef|#include|#line|#error|#warning|#pragma|#defined|#__has_include|#__has_cpp_attribute|#export|#import|#module {
            printf("<span class=\"preprocesor\">%s</span>", yytext);
            }

{COMMENT} printf("<span class=\"comment\">%s</span>", yytext);

"<"{ID}">"|"<"{ID}"."[a-zA-Z0-9_$]*">" printf("<span class=\"inclusion\">&lt;%s</span>", yytext+1);

{ID}    printf("<span class=\"identifier\">%s</span>", yytext);

{CHARSEQ}|{STR}        printf("<span class=\"string\">%s</span>", yytext);

"."|"+"|"-"|"*"|"/"|"="|"!"|">"|"<"|"!="|"=="|">="|"<="|"++"|"--"|"%"|"+="|"-="|"*="|"/="|"%="|">>="|"<<="|"&="|"^="|"|="|"&&"|"||"|"&"|"|"|"^"|"~"|"<<"|">>"   printf("<span class=\"operator\">%s</span>", yytext);

[;(){}:? \t\n,\[\]]+          printf("%s", yytext); /* echo the rest */

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

    puts(
        "<!doctype>"
        "<html>"
        "<head>"
        "    <title>");
        puts(argv[0]);
        puts("</title>"
        "    <style>"
        "        .keyword {"
        "            color: blue;"
        "        }"
        "        .number {"
        "            color: red;"
        "        }"
        "        .string {"
        "            color: orange;"
        "        }"
        "        .operator {"
        "            font-weight: 900;"
        "        }"
        "        .preprocesor {"
        "            color: purple;"
        "        }"
        "        .comment {"
        "            color: green;"
        "        }"
        "        .inclusion {"
        "            color: teal;"
        "        }"
        "    </style>"
        "</head>"
        "<body>"
        "    <pre class=\"code\">"
        );
    yylex();
    puts("</pre></body></html>");
    if ( argc > 0 )
            fclose(yyin);
    return 0;
}