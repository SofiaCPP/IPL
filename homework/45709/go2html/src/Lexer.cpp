//
// Created by Marty Kostov on 20.10.22.
//

#include "Lexer.hpp"

#include <utility>

#define RETURN_SUCCESS(ret) generationState = State::Success; return ret
#define RETURN_FAIL(ret) generationState = State::Fail; return ret
#define RETURN_ERROR(ret) generationState = State::Error; return ret

Lexer::Lexer(std::string code) : line(0), column(0), current(0), code(std::move(code)),
    generationState(State::Fail),
    keywordsTable({
       "break", "default", "func", "interface", "select",
       "case", "defer", "go", "map", "struct",
       "chan", "else", "goto", "package", "switch",
       "const", "fallthrough", "if", "range", "type",
       "continue", "for", "import", "return", "var"
    }),
    typesTable({
        "bool",
        "string",
        "int", "int8", "int16", "int32", "int64",
        "uint", "uint8", "uint16", "uint32", "uint64", "uintptr",
        "byte",
        "rune",
        "float32", "float64",
        "complex64", "complex128"
    }) {}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;
    Token token;

    do {
        token = nextToken();
        if (token.type != TokenType::End) {
            tokens.emplace_back(token);
        }

    } while (token.type != TokenType::End);

    return tokens;
}

Token Lexer::nextToken() {
    if (isEnd(code[current])) {
        return produceToken(TokenType::End, "");
    }

    const auto& whitespaces = parseWhitespaces();
    if (isStateSuccess()) {
        return produceToken(TokenType::Whitespaces, whitespaces);
    }

    const auto& comment = parseComment();
    if (isStateError()) {
        return produceToken(TokenType::Invalid, comment);

    } else if (isStateSuccess()) {
        return produceToken(TokenType::Comment, comment);
    }

    const auto& anOperator = parseOperator();
    if (isStateSuccess()) {
        return produceToken(TokenType::Operator, anOperator);
    }


    const auto& number = parseNumber();
    if (isStateSuccess()) {
        return produceToken(TokenType::Number, number);
    }

    const auto& string = parseString();
    if (isStateSuccess()) {
        return produceToken(TokenType::String, string);

    } else if (isStateError()) {
        return produceToken(TokenType::Invalid, string);
    }

    const auto& specialString = parseSpecialString();
    if (isStateSuccess()) {
        return produceToken(TokenType::String, specialString);

    } else if (isStateError()) {
        return produceToken(TokenType::Invalid, string);
    }

    const auto& keyword = parseKeyword();
    if (isStateSuccess())
    {
        return produceToken(TokenType::Keyword, keyword);
    }

    const auto& type = parseType();
    if (isStateSuccess())
    {
        return produceToken(TokenType::Type, type);
    }

    const auto& identifier = parseIdentifier();
    if (isStateSuccess())
    {
        return produceToken(TokenType::Identifier, identifier);
    }

    nextSymbol();
    return produceToken(TokenType::Invalid, std::string(&code[current-1], &code[current]));
}

bool Lexer::isEnd(char c) {
    return c == '\0';
}

Token Lexer::produceToken(TokenType type, const std::string& value) {
    auto token = Token{ type, this->line, this->lastTokenPosition, value };
    return token;
}

std::string Lexer::parseComment() {
    auto start = current;

    if (!match('/'))
    {
        RETURN_FAIL("");
    }

    if (match('/'))
    {
        while ( !isEnd(code[current]) && !isNewLine(code[current]))
        {
            nextSymbol();
        }
        RETURN_SUCCESS(std::string(&code[start], &code[current]));
    }
    else if (!match('*'))
    {
        previousSymbol();
        RETURN_FAIL("");
    }

    while (!isEnd(code[current]))
    {
        if (match('*') && match('/'))
        {
            RETURN_SUCCESS(std::string(&code[start], &code[current]));
        }
        nextSymbol();
    }

    RETURN_ERROR("/");
}

bool Lexer::match(char c) {
    if (c == code[current]) {
        nextSymbol();
        return true;
    }
    return false;
}


bool Lexer::isNewLine(char c) {
    return c == '\n';
}

void Lexer::nextSymbol() {
    ++current;
    ++column;
}

void Lexer::previousSymbol() {
    --current;
    --column;
}

bool Lexer::isStateError() {
    return generationState == State::Error;
}

bool Lexer::isStateSuccess() {
    return generationState == State::Success;
}

void Lexer::nextLine() {
    ++current;
    ++line;
    lastTokenPosition = column = 0;
}

std::string Lexer::parseNumber() {
    if (!isDigit(code[current])) {
        RETURN_FAIL("");
    }
    
    auto start = current;
    bool haveDot = false;

    while (!isEnd(code[current]) && !isNewLine(code[current]) &&
            isDigit(code[current]) || code[current] == '.' && !haveDot) {
        if (code[current] == '.') {
            haveDot = true;
        }
        nextSymbol();
    }

    RETURN_SUCCESS(std::string(&code[start], &code[current]));
}

bool Lexer::isDigit(char c) {
    return c >= '0' && c <= '9';
}

std::string Lexer::parseString() {
    if (!isStringBound(code[current])) {
        RETURN_FAIL("");
    }

    char bound = code[current];
    auto start = current;

    // skip first " or '
    nextSymbol();
    while (code[current] != bound && !isEnd(code[current]) && !isNewLine(code[current])) {
        nextSymbol();
    }

    if (isEnd(code[current]) || isNewLine(code[current])) {
        RETURN_ERROR(std::string(&code[start], &code[current]));
    }

    // skip second " or '
    nextSymbol();

    RETURN_SUCCESS(std::string(&code[start], &code[current]));
}

bool Lexer::isStringBound(char c) {
    return c == '\'' || c == '"';
}

std::string Lexer::parseSpecialString() {

    if (!isSpecialStringBound(code[current])) {
        RETURN_FAIL("");
    }

    char bound = code[current];
    auto start = current;

    // skip first `
    nextSymbol();
    while (code[current] != bound && !isEnd(code[current])) {
        nextSymbol();
    }

    if (isEnd(code[current])) {
        RETURN_ERROR(std::string(&code[start], &code[current]));
    }

    // skip second `
    nextSymbol();

    RETURN_SUCCESS(std::string(&code[start], &code[current]));
}

bool Lexer::isSpecialStringBound(char c) {
    return c == '`';
}

std::string Lexer::parseKeyword() {
    if (!isLowerCase(code[current])) {
        RETURN_FAIL("");
    }

    auto start = current;

    nextSymbol();
    while (isLowerCase(code[current]))
    {
        nextSymbol();
    }

    auto keyword = std::string(&code[start], &code[current]);
    if(keywordsTable.find(keyword) == keywordsTable.end()) {
        // It's not a keyword, so we must revert current counter
        column -= current - start;
        current = start;

        RETURN_FAIL("");
    }

    RETURN_SUCCESS(keyword);
}

bool Lexer::isLowerCase(char c) {
    return c >= 'a' && c <= 'z';
}

std::string Lexer::parseIdentifier() {
    if (!isValidIdentifierStartingChar(code[current])) {
        RETURN_FAIL("");
    }

    auto start = current;

    nextSymbol();
    while (isValidIdentifierChar(code[current])) {
        nextSymbol();
    }

    RETURN_SUCCESS(std::string(&code[start], &code[current]));
}

bool Lexer::isValidIdentifierStartingChar(char c) {
    return isUpperCase(c) || isLowerCase(c) || c == '_';
}

bool Lexer::isUpperCase(char c) {
    return c >= 'A' && c <= 'Z';
}

bool Lexer::isValidIdentifierChar(char c) {
    return isValidIdentifierStartingChar(c) || isDigit(c);
}

std::string Lexer::parseOperator() {
    switch (code[current]) {
        case '(' : nextSymbol(); RETURN_SUCCESS("(");
        case ')' : nextSymbol(); RETURN_SUCCESS(")");
        case '{' : nextSymbol(); RETURN_SUCCESS("{");
        case '}' : nextSymbol(); RETURN_SUCCESS("}");
        case '[' : nextSymbol(); RETURN_SUCCESS("[");
        case ']' : nextSymbol(); RETURN_SUCCESS("]");
        case '-' : nextSymbol(); RETURN_SUCCESS(match('-') ? "--" : match('>') ? "->" : "-");
        case '+' : nextSymbol(); RETURN_SUCCESS(match('+') ? "++" : "+");
        case '.' : nextSymbol(); RETURN_SUCCESS(".");
        case '!' : nextSymbol(); RETURN_SUCCESS(match('=') ? "!=" : "!");
        case '~' : nextSymbol(); RETURN_SUCCESS("~");
        case '*' : nextSymbol(); RETURN_SUCCESS("*");
        case '&' : nextSymbol(); RETURN_SUCCESS(match('&') ? "&&" : "&");
        case '/' : nextSymbol(); RETURN_SUCCESS("/");
        case '%' : nextSymbol(); RETURN_SUCCESS("%");
        case '>' : nextSymbol(); RETURN_SUCCESS(match('=') ? ">=" : match('>') ? ">>" : ">");
        case '<' : nextSymbol(); RETURN_SUCCESS(match('=') ? "<=" : match('<') ? "<<" :"<");
        case '^' : nextSymbol(); RETURN_SUCCESS("<");
        case '|' : nextSymbol(); RETURN_SUCCESS(match('|') ? "||" :"|");
        case '=' : nextSymbol(); RETURN_SUCCESS(match('=') ? "==" : "=");
        case ':' : nextSymbol(); RETURN_SUCCESS(match('=') ? ":=" : ":");
        case ',' : nextSymbol(); RETURN_SUCCESS(",");
        case ';' : nextSymbol(); RETURN_SUCCESS(";");
    }
    RETURN_FAIL("");
}

std::string Lexer::parseWhitespaces() {
    std::string whitespaces;

    while (code[current] == '\n' || code[current] == ' ' || code[current] == '\t') {
        if (code[current] == '\n') {
            nextLine();
            whitespaces += "\n";
        } else if(code[current] == ' ') {
            nextSymbol();
            whitespaces += " ";
        } else if(code[current] == '\t') {
            nextSymbol();
            auto endColumn = (((column - 1) / 4) + 1) * 4;
            for (unsigned i = column; i < endColumn; ++i) {
                whitespaces += " ";
            }
            whitespaces += " ";
        }
    }

    if (whitespaces.empty()) {
        RETURN_FAIL("");
    } else {
        RETURN_SUCCESS(whitespaces);
    }

}

std::string Lexer::parseType() {
    if (!isLowerCase(code[current])) {
        RETURN_FAIL("");
    }

    auto start = current;

    nextSymbol();
    while (isLowerCase(code[current]))
    {
        nextSymbol();
    }

    while (isDigit(code[current]))
    {
        nextSymbol();
    }

    auto keyword = std::string(&code[start], &code[current]);
    if(typesTable.find(keyword) == typesTable.end()) {
        // It's not a keyword, so we must revert current counter
        column -= current - start;
        current = start;

        RETURN_FAIL("");
    }

    RETURN_SUCCESS(keyword);
}
