/* scanner for a C++ language */

%{
#include "cpp2html.tab.h"
#define YY_NO_UNISTD_H
char* current_str = 0;
int openbracketCounter = 0;
%}

DIGIT    [0-9]
ID       [a-zA-Z_$][a-zA-Z0-9_$]*
CHARSEQ  '([^\']|(\\\'))*'
STR      \"([^\"]|(\\\"))*\"
COMMENT  \/\/.*\n|\/\*.*\*\/

%%

{DIGIT}+    {
            current_str = yytext;
            return NUMBER;
            }

{DIGIT}+"."{DIGIT}*        {
            current_str = yytext;
            return NUMBER;
            }

alignas|alignof|and|and_eq|asm|atomic_cancel|atomic_commit|atomic_noexcept|auto|bitand|bitor|bool|break|case|catch|char|char8_t|char16_t|char32_t|class|compl|concept|const|consteval|constexpr|constinit|const_cast|continue|co_await|co_return|co_yield|decltype|default|delete|do|double|dynamic_cast|else|enum|explicit|export|extern|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|noexcept|not|not_eq|nullptr|operator|or|or_eq|private|protected|public|reflexpr|register|reinterpret_cast|requires|return|short|signed|sizeof|static|static_assert|static_cast|struct|switch|synchronized|template|this|thread_local|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile|wchar_t|while|xor|xor_eq {
            current_str = yytext;
            return KEYWORD;
            }

#if|#elif|#else|#endif|#ifdef|#ifndef|#elifdef|#elifndef|#define|#undef|#include|#line|#error|#warning|#pragma|#defined|#__has_include|#__has_cpp_attribute|#export|#import|#module {
            current_str = yytext;
            return PREPROCESOR;
            }

{COMMENT} {
            current_str = yytext;
            return COMMENT;
            }

"<"{ID}">"|"<"{ID}"."[a-zA-Z0-9_$]*">" {
            current_str = yytext;
            return INCLUSION;
            }

{ID}    {
            current_str = yytext;
            return IDENTIFIER;
            }

{CHARSEQ}|{STR}        {
            current_str = yytext;
            return STRING;
            }

"."|"+"|"-"|"*"|"/"|"="|"!"|">"|"<"|"!="|"=="|">="|"<="|"++"|"--"|"%"|"+="|"-="|"*="|"/="|"%="|">>="|"<<="|"&="|"^="|"|="|"&&"|"||"|"&"|"|"|"^"|"~"|"<<"|">>"   {
            current_str = yytext;
            return OPERATOR;
    }

[():? ,\[\]]+          {
            current_str = yytext;
            return OTHER;
            }

"{"          {
            ++openbracketCounter;
            current_str = yytext;
            return OBR;
            }

"}"          {
            --openbracketCounter;
            current_str = yytext;
            return CBR;
            }
            
";"          {
            current_str = yytext;
            return SEMI;
            }

"\n"          {
            current_str = yytext;
            return NL;
            }

"\t"          {
            current_str = yytext;
            return TB;
            }

.           {
            current_str = yytext;
            return UNKNOWN;
            }

%%

int yywrap()
{
    return 1;
}
