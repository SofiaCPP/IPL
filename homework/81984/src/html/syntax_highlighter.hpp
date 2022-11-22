#ifndef __HTML_SYNTAX_HIGHLIGHTER_HPP__
#define __HTML_SYNTAX_HIGHLIGHTER_HPP__

#include "../lexer/lexer.hpp"
#include "token_color.hpp"

struct SyntaxHighlighterOptions
{
  bool hover_information;
};

class SyntaxHighlighter
{
  Map<TokenType, TokenColor> pallete_;
  SyntaxHighlighterOptions options_;
public:
  SyntaxHighlighter(SyntaxHighlighterOptions options = { .hover_information = false });

  void ProcessFile(String file_name);
  String ProcessToken(Token token);
private:
  String ProcessTokens(Vector<Token>& tokens);
  String ProcessTag(Token& token);
  String ProcessTagType(Token& token);
  String ProcessTagData(Token& token);
  String ProcessAttributes(Token& token);
  String ProcessTokenType(Token& token);
  String ProcessTokenColor(Token& token);
  String ProcessTokenData(Token& token);
};

#endif
