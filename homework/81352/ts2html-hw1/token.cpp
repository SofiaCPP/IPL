#include "token.h"

Token::Token(std::string text, TokenType type): text(text), type(type) {}

std::string Token::getTypeName() {
    switch(this->type) {
        case KEYWORD: return "keyword";
        case IDENTIFIER: return "identifier";
        case NUMBER: return "number";
        case OPERATOR: return "operator";
        case STRING: return "string";
        case OTHER: return "other";
        case SEMICOLON: return "semicolon";
        case LEFT_BLOCK_BRACKET:
        case RIGHT_BLOCK_BRACKET:
        case LEFT_BRACKET:
        case RIGHT_BRACKET: return "bracket";
        case NEWLINE:
        case WHITESPACE: return "whitespace";
        case DECORATOR: return "decorator";
        case COMMENT: return "comment";
        case UNRECOGNIZED: return "unrecognized";
    }
    return "";
}
std::string Token::getTextValue() {
    return this->text;
}

TokenType Token::getTokenType() {
    return this->type;
}
