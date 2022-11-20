#ifndef RUBY_IPL_LEXER_TOKEN_HPP_
#define RUBY_IPL_LEXER_TOKEN_HPP_

#include "../utils/types.hpp"

enum TokenType
{
  Identifier,
  Symbol,
  SingleQuotedString,
  DoubleQuotedString,
  Number,

  Class,
  Def,
  Do,
  Else,
  Elsif,
  End,
  If,
  Module,
  Unless,
  While,

  Comma,
  ClosedParenthesis,
  Dot,
  DoubleEqual,
  Equal,
  GreaterThan,
  GreaterThanOrEqual,
  LessThan,
  LessThanOrEqual,
  Octothorp,
  OpenParenthesis,
  StraightLine,

  NewLine,
  Space,
  EndOfFile,

  Unknown
};

struct Token
{
  TokenType type;
  String data;
  Index line;
  Index column;
};

#endif
