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

  expression_ptr parse_program_expression();
private:
  expression_ptr parse_statement();
  expression_ptr parse_expression();

  expression_ptr parse_identifier();

  expression_ptr parse_if_statement(expression_ptr body = nullptr);
  expression_ptr parse_case_statement();
  expression_ptr parse_when_expression();
  expression_ptr parse_while_statement(expression_ptr body = nullptr);
  expression_ptr parse_for_statement();
  expression_ptr parse_class_statement();
  expression_ptr parse_function_definition_statement();
  expression_ptr parse_call_expression();
  expression_ptr parse_block();

  expression_ptr parse_literal_expression();
  expression_ptr parse_literal_nil();
  expression_ptr parse_literal_number();
  expression_ptr parse_literal_string();
  expression_ptr parse_literal_symbol();
  expression_ptr parse_literal_list();
  expression_ptr parse_literal_hash();
  expression_ptr parse_literal_hash_element();

  string assert_next(token_type type, token_type backup_type = UNRECOGNIZED);
  bool is_unary_operator();
  bool is_binary_operator();
};

#endif
