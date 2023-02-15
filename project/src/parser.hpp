#ifndef PARSER_HPP
#define PARSER_HPP

#include "token/token.hpp"
#include "expression/expression.hpp"

class parser
{
  vector<token> m_tokens;
  uint m_position;
public:
  parser(const vector<token>& tokens);

  ptr<expression> parse_program_expression();
private:
  ptr<expression> parse_statement();
  ptr<expression> parse_expression();

  ptr<expression> parse_class_expression();
  ptr<expression> parse_function_expression();
  ptr<expression> parse_identifier_expression();

  ptr<expression> parse_arguments_expression();
  ptr<expression> parse_call_expression();
  ptr<expression> parse_block_expression();

  ptr<expression> parse_if_expression(ptr<expression> body = nullptr);
  ptr<expression> parse_case_expression();
  ptr<expression> parse_when_expression();

  ptr<expression> parse_while_expression(ptr<expression> body = nullptr);
  ptr<expression> parse_for_expression();

  ptr<expression> parse_literal_expression();
  ptr<expression> parse_hash_expression();
  ptr<expression> parse_hash_element_expression();
  ptr<expression> parse_list_expression();
  ptr<expression> parse_string_expression();
  ptr<expression> parse_symbol_expression();
  ptr<expression> parse_number_expression();
  ptr<expression> parse_boolean_expression();
  ptr<expression> parse_nil_expression();

  token current_token();
  string assert_token(token_type type, token_type backup_type = UNRECOGNIZED);

  bool is_current_token_unary_operator();
  bool is_current_token_binary_operator();
};

#endif
