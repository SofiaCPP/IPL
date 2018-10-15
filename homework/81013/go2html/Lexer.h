#pragma once

#include <string>

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

    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    BitClear,
    ShiftLeft,
    ShiftRight,

    PlusEqual,
    MinusEqual,
    MultEqual,
    DivisionEqual,
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
    Assignment,
    NOT,

    NotEqual,
    LessEqual,
    GreaterEqual,
    ShortAssignment,
    Variadic,

    LeftParen,
    RightParen,
    LeftSquareBracket,
    RightSquareBracket,
    LeftBrace,
    RightBrace,

    Coma,
    Dot,

    Semicolon,
    Colon,

    // Identifier
    Identifier,

    // Types
    DecimalInteger,
    Float,
    ImaginaryNum,
    String,
    // -> runes(?)

    Nil
};

struct Token
{
    TokenType Type;
    unsigned Line;
    std::string Lexeme;
    double Number;
};
