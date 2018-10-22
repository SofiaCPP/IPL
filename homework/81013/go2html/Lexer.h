#pragma once

#include "CommonTypes.h"

enum class TokenType
{
    // Keywords
    Break,
    Default,
    Func,
    Interface,
    Select,
    Case,
    Defer,
    Go,
    Map,
    Struct,
    Chan,
    Else,
    Goto,
    Package,
    Switch,
    Const,
    Fallthrough,
    If,
    Range,
    Type,
    Continue,
    For,
    Import,
    Return,
    Var,

    // Operators & Punctuation
    Plus,
    Minus,
    Star,
    Division,
    Modulo,

	NOT,
    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    BitClear,
    ShiftLeft,
    ShiftRight,

    PlusEqual,
    MinusEqual,
    MultEqual,
    DivEqual,
    ModuloEqual,

    // -> skipped bitwise + equal stuff

    ConditionalAND,
    ConditionalOR,
    ReceiveOperator,
    PlusPlus,
    MinusMinus,

    Equal,
    Less,
    Greater,

    NOTEqual,
    LessEqual,
    GreaterEqual,

	Assignment,
	ShortAssignment,

    Variadic,

    LeftParen,
    RightParen,
    LeftSquareBracket,
    RightSquareBracket,
    LeftBrace,
    RightBrace,

    Comma,
    Dot,

    Semicolon,
    Colon,

    // Identifier
    Identifier,

    // Types
    DecimalInteger, // not really supported ATM
    Float,
    ImaginaryNum, // not really supported ATM
    String,
    // -> runes(?)

    True,
    False,
    Nil,

    Whitespace,
    Comment,

    Eof,
    Invalid
};

struct Token
{
    TokenType Type;
    unsigned Line;
    IPLString Lexeme;
    double Number;
};

struct LexerSettings
{
	bool CreateWhitespaceTokens;
	bool CreateCommentTokens;
};

IPLResult Tokenize(const char* code, IPLVector<Token>& tokens, const LexerSettings& settings);
IPLResult Tokenize(const char* code, IPLVector<Token>& tokens);