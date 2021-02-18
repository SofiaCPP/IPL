#ifndef _LEX
#define _LEX
#include <string>
#include <vector>
#include <unordered_map>
#include <fstream>

enum class TokenType
{
    Identifier,
    Number,
    StringLit,
    Comment,
    Invalid,
    Whitespace,

    //operators and punctuation
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    BitLogicalAnd, // &
    BitLogicalOr,  // |
    BitLogicalXor, // ^
    BitAndNot,     // &^
    LeftShift,     // <<
    RightShift,    // >>
    PlusEqual,
    MinusEqual,
    MultiplyEqual,
    DivideEqual,
    ModuloEqual,
    And,
    Or,
    Receive, //<-
    PlusPlus,
    MinusMinus,
    Equals,
    Smaller,
    Greater,
    SmallerEquals,
    GreaterEquals,
    Assignent,
    Declare,
    Not,
    NotEqual,
    ThreeDots,
    LeftBrace,
    RightBrace,
    LeftSquareBrace,
    RightSquareBrace,
    LeftCurlyBrace,
    RightCurlyBrace,
    Comma,
    Semicolon,
    Dot,
    Colon,

    //Keywords
    Break,
    Case,
    Chan,
    Const,
    Continue,
    Default,
    Defer,
    Else,
    Fallthrough,
    For,
    Func,
    Go,
    GoTo,
    If,
    Import,
    Interface,
    Map,
    Package,
    Range,
    Return,
    Select,
    Struct,
    Switch,
    Type,
    Var,
    Nil,
    True,
    False,
    Int,
    String,
    Float,
    Bool
};

struct Token
{
    TokenType type;
    std::string lexeme;
};

struct Helper
{
    static bool isLetter(char c);
    static bool isCapitalLetter(char c);
    static bool isSmallLetter(char c);

    static bool isDigit(char c);
    static bool isEof(char c);
    static bool isNewLine(char c);
    static bool isHexLiteral(char c);
    static bool isHexDigit(char c);
    static bool isBinLiteral(char c);
    static bool isBinDigit(char c);
    static bool isDot(char c);
    static bool isOctalLiteral(char c);
    static bool isOctalDigit(char c);
    static bool isStringLiteral(char c);
    static bool isValidIdentifierStart(char c);
    static bool isValidIdentifierLetter(char c);
};

struct LexerResult
{
    std::vector<Token> tokens;
};

class Tokenizer
{
private:
    const char *code;
    size_t index;
    bool state;
    std::unordered_map<std::string, TokenType> keywordTable;

public:
    Tokenizer(const char *_code);

    LexerResult tokenize();

    bool match(char c);
    void nextSymbol();
    void previousSymbol();

    std::string parseNumber();
    std::string parseComment();
    std::string parseString();
    std::string parseKeyword();
    std::string parseIdentifier();
    std::string parseWhitespace();

    Token nextToken();
};

struct GenerateHTML
{
    static void generate(LexerResult &lr, const std::string &filename = "go2html.html");
};

#endif