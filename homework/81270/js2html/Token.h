//
// Created by bmestanov on 23.10.18.
//

#ifndef ES2HTML_LEXER_H
#define ES2HTML_LEXER_H

#include <vector>
#include <string>
#include <functional>
#include "Token.h"
#include "Common.h"

enum TokenType {
    Whitespace,
    NewLine,
    Raw,
    Comment,
    Keyword,
    BooleanLiteral,
    NumericLiteral,
    StringLiteral,
    TemplateLiteral,
    Regex,
    Identifier,
    Global
};

struct Token {
    TokenType type;
    IPLString lexeme;

    Token(const TokenType &type, const char *lexeme);
};


#endif //ES2HTML_LEXER_H
