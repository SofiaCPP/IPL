%{
    #include "definitions.h"
%}

whitespace [ \r\t\n]+
directive  "#".*

comment_single "//".*
comment_multi [/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]  

comment ({comment_single})|({comment_multi})

digit10 [0-9]
digit16 [0-9a-fA-F]
digit8  [0-7]

exponent [eE][+-]?{digit10}+

literal_int         ([1-9]({digit10}*))|("0x"({digit16}+))|("0"({digit8}*))
literal_float       {digit10}*"."{digit10}+{exponent}?
literal_char        '([^\']|(\\\'))'
literal_string      \"(\\.|[^"\\])*\"

standard_typedef    ("char16_t"|"char32_t"|"wchar_t"|"while"|"int8_t"|"uint8_t"|"int16_t"|"uint16_t"|"int32_t"|"uint32_t"|"int64_t"|"uint64_t"|"int_least8_t"|"uint_least8_t"|"int_least16_t"|"uint_least16_t"|"int_least32_t"|"uint_least32_t"|"int_least64_t"|"uint_least64_t"|"int_fast8_t"|"uint_fast8_t"|"int_fast16_t"|"uint_fast16_t"|"int_fast32_t"|"uint_fast32_t"|"int_fast64_t"|"uint_fast64_t"|"intptr_t"|"uintptr_t"|"intmax_t"|"uintmax_t"|"wint_t"|"wchar_t"|"wctrans_t"|"wctype_t"|"size_t"|"time_t")
keyword             ("alignas"|"alignof"|"asm"|"auto"|"bool"|"break"|"case"|"catch"|"char"|"class"|"const"|"const_cast"|"constexpr"|"continue"|"decltype"|"default"|"delete"|"do"|"double"|"dynamic_cast"|"else"|"enum"|"explicit"|"export"|"extern"|"false"|"final"|"float"|"for"|"friend"|"goto"|"if"|"inline"|"int"|"long"|"mutable"|"namespace"|"new"|"noexcept"|"nullptr"|"operator"|"override"|"private"|"protected"|"public"|"register"|"reinterpret_cast"|"return"|"short"|"signed"|"sizeof"|"static"|"static_assert"|"static_cast"|"struct"|"switch"|"template"|"this"|"thread_local"|"throw"|"true"|"try"|"typedef"|"typeid"|"typename"|"union"|"unsigned"|"using"|"virtual"|"void"|"volatile")

identifier          [a-zA-Z_][a-zA-Z0-9_]*


unknown             (.)

%% /*-----------------------------------------------------------------------------------------------*/

{whitespace}            {return TT_WHITESPACE;}
{comment}               {return TT_COMMENT;}
{keyword}               {return TT_KEYWORD;}
{standard_typedef}      {return TT_STANDARD_TYPEDEF;}
{identifier}            {return TT_IDENTIFIER;}
{literal_int}           {return TT_LITERAL_NUMERIC;}
{literal_float}         {return TT_LITERAL_NUMERIC;}
{literal_char}          {return TT_LITERAL_TEXT;}
{literal_string}        {return TT_LITERAL_TEXT;}
{directive}             {return TT_DIRECTIVE;}  
{unknown}               {return TT_UNKNOWN;}
%%

int yywrap(){
    return 1;
}

void lex()
{
    int tokenType;
    while(tokenType = yylex()) {
        handle_token((token_type)tokenType, yytext);
    }
}
