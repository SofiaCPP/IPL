#ifndef TOKEN
#define TOKEN

#include <string>

enum TokenType {
    KEYWORD,
    IDENTIFIER,
    NUMBER,
    OPERATOR,
    STRING,
    SEMICOLON,
    LEFT_BLOCK_BRACKET,
    RIGHT_BLOCK_BRACKET,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    NEWLINE,
    WHITESPACE,
    DECORATOR,
    COMMENT,
    OTHER,
    UNRECOGNIZED
};
        
class Token {
    private: 
        std::string text;
        TokenType type;
    public:
        Token(std::string, TokenType);
        std::string getTypeName();
        std::string getTextValue();
        TokenType getTokenType();
};
#endif