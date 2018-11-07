#include "Token.h"

Token::Token(const TokenType &type, const char *lexeme) :
        type(type), lexeme(lexeme) {
}
