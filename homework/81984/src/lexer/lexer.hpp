#ifndef RUBY_IPL_LEXER_HPP_
#define RUBY_IPL_LEXER_HPP_

#include "../utils/types.hpp"
#include "token.hpp"

struct LexerOptions
{
  bool tokenize_whitespaces;
  bool store_all_data;
};

class Lexer
{
  Index line_;
  Index column_;
  Map<String, TokenType> keywords_;
  LexerOptions options_;
public:
  Lexer(LexerOptions options = { .tokenize_whitespaces = false, .store_all_data = false });

  Vector<Token> TokenizeExpression(CString& expression);
private:
  Token ReadNextToken(CString& expression);

  void ReadWhitespace(CString& expression, Token& token);
  void ReadSpecialCharacter(CString& expression, Token& token);
  void ReadKeyword(CString& expression, Token& token);
  void ReadIdentifier(CString& expression, Token& token);
  void ReadSymbol(CString& expression, Token& token);
  void ReadString(CString& expression, Token& token);
  void ReadNumber(CString& expression, Token& token);

  void ClearWhitespaces(CString& expression);
  String MatchRegex(CString& expression, Regex regex);
};

#endif
