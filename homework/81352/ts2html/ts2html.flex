%{
#include <vector>
#include <iostream>
#include "token.h"
#include "highlighter.h"
std::vector<Token> tokens = std::vector<Token>();
%}

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
    tokens.push_back(Token(yytext, TokenType::COMMENT));
}

break|case|catch|class|const|continue|debugger|default|delete|do|else|enum|export|extends|false|finally|for|function|if|import|in|instanceof|new|null|return|super|switch|this|throw|true|try|typeof|var|void|while|with|as|implements|interface|let|package|private|protected|public|static|yield|any|boolean|constructor|declare|get|module|require|number|set|string|symbol|type|from|of {
    tokens.push_back(Token(yytext, TokenType::KEYWORD));
}

{DECORATOR} {
    tokens.push_back(Token(yytext, TokenType::DECORATOR));
}

{STRQ}|{STRDQ} {
    tokens.push_back(Token(yytext, TokenType::STRING));
}

{DIGIT}+ {
    tokens.push_back(Token(yytext, TokenType::NUMBER));
}

{DIGIT}+"."{DIGIT}* {
    tokens.push_back(Token(yytext, TokenType::NUMBER));
}

{ID} {
    tokens.push_back(Token(yytext, TokenType::IDENTIFIER));
}

":"|"?"|"."|"!"|"~"|"-"|"+"|"++"|"--"|"*"|"/"|"%"|"<<"|">>"|">>>"|"<"|"<="|">"|">="|"=="|"!="|"==="|"!=="|"&"|"^"|"|"|"&&"|"||"|"="|"+="|"-="|"*="|"/="|"%="|"<<="|">>="|">>>="|"&="|"^="|"|="|"=>" {
    tokens.push_back(Token(yytext, TokenType::OPERATOR));
}

";" {
    tokens.push_back(Token(yytext, TokenType::SEMICOLON));
}

{LEFT_BLOCK_BRACKET} {
    tokens.push_back(Token(yytext, TokenType::LEFT_BLOCK_BRACKET));
}

{RIGHT_BLOCK_BRACKET} {
    tokens.push_back(Token(yytext, TokenType::RIGHT_BLOCK_BRACKET));
}

{LEFT_BRACKET} {
    tokens.push_back(Token(yytext, TokenType::LEFT_BRACKET));
}

{RIGHT_BRACKET} {
    tokens.push_back(Token(yytext, TokenType::RIGHT_BRACKET));
}

[\n] {
    tokens.push_back(Token(yytext, TokenType::NEWLINE));
}

[ \t]+ {
    tokens.push_back(Token(yytext, TokenType::WHITESPACE));
}

. {
    tokens.push_back(Token(yytext, TokenType::UNRECOGNIZED));
}

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
    yylex();
    Highlighter highlighter = Highlighter(tokens);
    std::cout << highlighter.highlight();
    return 0;
}