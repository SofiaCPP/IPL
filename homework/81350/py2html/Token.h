#pragma once
#include<string>
#include<unordered_map>

typedef std::string IPLString ;

 enum TokenType{
    False,
    None,
    True,
    And,
    As,
    Assert,
    Break,
    Class,
    Continue,
    Def,
    Del,
    Elif,
    Else,
    Except,
    Finally,
    For,
    From,
    Global,
    If,
    Import,
    In,
    Is,
    Lambda,
    Nonlocal,
    Not,
    Or,
    Pass,
    Print,
    Raise,
    Return,
    Try,
    While,
    With,
    Yield
 };

struct Token
{
    TokenType Type;
    unsigned Line;
    IPLString Lexeme;
    double Number = 0.0;
};

extern std::unordered_map<std::string, TokenType> keywords;
