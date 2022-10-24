//
// Created by Marty Kostov on 20.10.22.
//

#ifndef GO2HTML_TOKEN_HPP
#define GO2HTML_TOKEN_HPP

#include <string>

enum class TokenType
{
    Invalid,
    Operator,
    Keyword,
    Type,
    Number,
    String,
    Identifier,
    Comment,
    End,
};

struct Token {
    TokenType type;
    unsigned line;
    unsigned column;
    std::string value;
};

#endif //GO2HTML_TOKEN_HPP
