#ifndef LEXER_HPP
#define LEXER_HPP

#include "token/token.hpp"

class lexer
{
  token m_token;
  map<string, token_type> m_keywords;

  string m_expression;
  uint m_position;
public:
  lexer(const string& expression);

  vector<token> tokenize();
private:
  void read_next_token();

  void match_keyword_token();
  void match_identifier_token();
  void match_number_token();
  void match_string_token();
  void match_symbol_token();
  void match_character_combination_token();

  void clear_whitespaces();
  string match_regex(const regex& regex);
};

#endif
