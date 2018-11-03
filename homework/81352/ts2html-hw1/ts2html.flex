%{
#include <vector>
#include <iostream>
#include "token.h"
#include "highlighter.h"
%}

%option reentrant
%option extra-type="std::vector<Token> *"


DIGIT    [0-9]
ID       [a-zA-Z_$][a-zA-Z0-9_$]*
STRQ     '([^\']|(\\\'))*'
STRDQ    \"([^\"]|(\\\"))*\"
DECORATOR @[a-zA-Z]+
LEFT_BLOCK_BRACKET "{"
RIGHT_BLOCK_BRACKET "}"
LEFT_BRACKET "("
RIGHT_BRACKET ")"
SINGLE_LINE_COMMENT "//"[^\n]*
MULTI_LINE_COMMENT "/*".*"*/"  
COMMENT  {SINGLE_LINE_COMMENT}|{MULTI_LINE_COMMENT}
%%

{COMMENT} {
    yyextra->push_back(Token(yytext, TokenType::COMMENT));
}

break|case|catch|class|const|continue|debugger|default|delete|do|else|enum|export|extends|false|finally|for|function|if|import|in|instanceof|new|null|return|super|switch|this|throw|true|try|typeof|var|void|while|with|as|implements|interface|let|package|private|protected|public|static|yield|any|boolean|constructor|declare|get|module|require|number|set|string|symbol|type|from|of {
    yyextra->push_back(Token(yytext, TokenType::KEYWORD));
}

{DECORATOR} {
    yyextra->push_back(Token(yytext, TokenType::DECORATOR));
}

{STRQ}|{STRDQ} {
    yyextra->push_back(Token(yytext, TokenType::STRING));
}

{DIGIT}+ {
    yyextra->push_back(Token(yytext, TokenType::NUMBER));
}

{DIGIT}+"."{DIGIT}* {
    yyextra->push_back(Token(yytext, TokenType::NUMBER));
}

{ID} {
    yyextra->push_back(Token(yytext, TokenType::IDENTIFIER));
}

":"|"?"|"."|"!"|"~"|"-"|"+"|"++"|"--"|"*"|"/"|"%"|"<<"|">>"|">>>"|"<"|"<="|">"|">="|"=="|"!="|"==="|"!=="|"&"|"^"|"|"|"&&"|"||"|"="|"+="|"-="|"*="|"/="|"%="|"<<="|">>="|">>>="|"&="|"^="|"|="|"=>" {
    yyextra->push_back(Token(yytext, TokenType::OPERATOR));
}

";" {
    yyextra->push_back(Token(yytext, TokenType::SEMICOLON));
}

{LEFT_BLOCK_BRACKET} {
    yyextra->push_back(Token(yytext, TokenType::LEFT_BLOCK_BRACKET));
}

{RIGHT_BLOCK_BRACKET} {
    yyextra->push_back(Token(yytext, TokenType::RIGHT_BLOCK_BRACKET));
}

{LEFT_BRACKET} {
    yyextra->push_back(Token(yytext, TokenType::LEFT_BRACKET));
}

{RIGHT_BRACKET} {
    yyextra->push_back(Token(yytext, TokenType::RIGHT_BRACKET));
}

[\n] {
    yyextra->push_back(Token(yytext, TokenType::NEWLINE));
}

[ \t]+ {
    yyextra->push_back(Token(yytext, TokenType::WHITESPACE));
}

. {
    yyextra->push_back(Token(yytext, TokenType::UNRECOGNIZED));
}

%%

int yywrap(yyscan_t scanner)
{
    return 1;
}


int main(int argc, const char* argv[])
{   
    ++argv, --argc;  /* skip over program name */
    FILE* in;
    if ( argc > 0 )
            in = fopen( argv[0], "r" );
    else
            in = stdin;
    std::vector<Token> tokens = std::vector<Token>();
    yyscan_t scanner;
    yylex_init_extra(&tokens, &scanner);
    yyset_in( in, scanner );
    yylex(scanner);
    yylex_destroy(scanner);
    Highlighter highlighter = Highlighter(tokens);
    std::cout << highlighter.highlight();
    return 0;
}