#ifndef LEXER_H
#define LEXER_H

#include <vector>
#include <map>
#include <string>

enum TokenType
{
// Single character tokens
    LEFT_PAREN = 1,         // (
    RIGHT_PAREN,        // )
    LEFT_BRACKET,       // [
    RIGHT_BRACKET,      // ]
    LEFT_CUR_BRAC,      // {
    RIGHT_CUR_BRAC,     // }
    PLUS,               // +
    MINUS,              // -
    MULTIPLICATION,     // *
    DIVIDE,             // /
    MODULO,             // %
    LESSER,             // <
    GREATER,            // >
    ASSIGN,             // =
    SEMI_COLON,         // ;
    QUESTION_MARK,      // ?
    EXCLAMATION_MARK,   // !
    COLON,              // :
    BITWISE_AND,        // &
    BITWISE_OR,         // |
    BITWISE_XOR,        // ^
    BITWISE_NOT,        // ~
    DOT,                // .

// Double character tokens
    SCOPE,              // ::
    PLUS_PLUS,          // ++
    MINUS_MINUS,        // --
    PTR_ARROW,          // ->
    LEFT_BITWISE_SHIFT, // <<
    RIGHT_BITWISE_SHIFT,// >>
    EQUAL,              // ==
    NOT_EQUAL,          // !=
    LESS_EQUAL,         // <=
    GREATER_EQUAL,      // >=
    LOGIC_AND,          // &&
    LOGIC_OR,           // ||
    MOD_EQUAL,          // %=
    MULT_EQUAL,         // *=
    DIV_EQUAL,          // /=
    PLUS_EQUAL,         // +=
    MINUS_EQUAL,        // -=
    BIT_AND_EQUAL,      // &=
    BIT_OR_EQUAL,       // |=
    BIT_XOR_EQUAL,      // ^=

// Three character tokens
    LEFT_SHIFT_EQUAL,   // <<=
    RIGHT_SHIFT_EQUAL,  // >>=

// Keyword Tokens
    ALIGNAS,
    ALIGNOF,
    AND,
    AND_EQ,
    ASM,
    AUTO,
    BITAND,
    BITOR,
    BOOL,
    BREAK,
    CASE,
    CATCH,
    CHAR,
    CHAR16_T,
    CHAR32_T,
    CLASS,
    COMPL,
    CONST,
    CONSTEXPR,
    CONST_CAST,
    CONTINUE,
    DECLTYPE,
    DEFAULT,
    DELETE,
    DO,
    DOUBLE,
    DYNAMIC_CAST,
    ELSE,
    ENUM,
    EXPLICIT,
    EXPORT,
    EXTERN,
    FALSE,
    FLOAT,
    FOR,
    FRIEND,
    GOTO,
    IF,
    INLINE,
    INT,
    LONG,
    MUTABLE,
    NAMESPACE,
    NEW,
    NOEXCEPT,
    NOT,
    NOT_EQ,
    NULLPTR,
    OPERATOR,
    OR,
    OR_EQ,
    PRIVATE,
    PROTECTED,
    PUBLIC,
    REGISTER,
    REINTERPRET_CAST,
    RETURN,
    SHORT,
    SIGNED,
    SIZEOF,
    STATIC,
    STATIC_ASSERT,
    STATIC_CAST,
    STRUCT,
    SWITCH,
    TEMPLATE,
    THIS,
    THREAD_LOCAL,
    THROW,
    TRUE,
    TRY,
    TYPEDEF,
    TYPEID,
    TYPENAME,
    UNION,
    UNSIGNED,
    USING,
    VIRTUAL,
    VOID,
    VOLATILE,
    WCHAR_T,
    WHILE,
    XOR,
    XOR_EQ,
    
    // preprocessors
    PRE_DEFINE,
    PRE_UNDEF,
    PRE_INCLUDE,
    PRE_IF,
    PRE_IFDEF,
    PRE_IFNDEF,
    PRE_ELSE,
    PRE_ELIF,
    PRE_ENDIF,
    PRE_LINE,
    PRE_ERROR,
    PRE_PRAGMA,

    NUMBER,
    IDENTIFIER,
    STRING,
    COMMENT,
    Eof,
    INVALID
};

enum State
{
    FAIL,
    SUCCESS,
    ERROR
};

struct Token
{
    TokenType type;
    unsigned line;
    std::string lexeme;
    double value;
};

class Tokenizer
{
private:
    const char* code;
    unsigned line;
    unsigned column;
    unsigned codeIndex;
    State state;
    std::map<std::string, TokenType> keywords;
    std::map<std::string, TokenType> preprocessors;

public:
    Tokenizer(const char* code);
    Token nextToken();
    Token produceToken(TokenType type, std::string lexeme, double val = 0.0);
    Token produceToken(TokenType type);

    bool isAtEOF() const;
    bool match(char c) const;
    bool isStateError() const;
    bool isStateSuccess() const;
    bool tokenize(std::vector<Token> &tokens);

    double parseNumber();
    std::string parseComment();
    std::string parseString();
    Token parseKeyword();
    Token parsePreprocessor();
    std::string parseIdentifier();

    void skipWhiteSpaces();
    void nextSymbol();
    void prevSymbol();

};

//#include "Lexer.cpp"

#endif

