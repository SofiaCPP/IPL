//
// Created by Marty Kostov on 20.10.22.
//

#ifndef GO2HTML_LEXER_HPP
#define GO2HTML_LEXER_HPP


#include <set>
#include <vector>
#include <string>

#include "Token.hpp"

enum class State : unsigned char {
    Success,
    Fail,
    Error
};

class Lexer {
    unsigned line;
    unsigned column;
    unsigned current;
    unsigned lastTokenPosition{};
    std::string code;

    State generationState;

    std::set<std::string> keywordsTable;
    std::set<std::string> typesTable;

public:
    Lexer(std::string code);
    std::vector<Token> tokenize();

private:
    Token nextToken();

    static bool isEnd(char c);
    static bool isNewLine(char c);
    static bool isDigit(char c);
    static bool isLowerCase(char c);
    static bool isUpperCase(char c);
    static bool isValidIdentifierStartingChar(char c);
    static bool isValidIdentifierChar(char c);
    static bool isStringBound(char c);
    static bool isSpecialStringBound(char c);

    bool match(char c);

    void nextSymbol();
    void nextLine();

    bool isStateError();
    bool isStateSuccess();


    Token produceToken(TokenType type, const std::string& value);





    void previousSymbol();


    std::string parseOperator();
    std::string parseComment();
    std::string parseNumber();
    std::string parseString();
    std::string parseSpecialString();


    std::string parseKeyword();


    std::string parseIdentifier();

    void skipWhitespaces();

    std::string parseType();

    std::string parseWhitespaces();
};


#endif //GO2HTML_LEXER_HPP
