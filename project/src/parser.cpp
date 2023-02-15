#include "parser.hpp"

parser::parser(const vector<token>& tokens) :
  m_tokens(tokens), m_position(0)
{
}

ptr<expression> parser::parse_program_expression()
{
  ptr<program_expression> program = create_pointer<program_expression>();

  while (
    m_position < m_tokens.size()    &&
    current_token().m_type != END   &&
    current_token().m_type != ELSIF &&
    current_token().m_type != ELSE  &&
    current_token().m_type != WHEN
  ) {
    if (current_token().m_type == NEW_LINE)
      ++m_position;
    else
      program->m_statements.push_back(parse_statement());
  }

  return program;
}

ptr<expression> parser::parse_statement()
{
  switch (current_token().m_type)
  {
  case CLASS:
  case MODULE: return parse_class_expression();
  case DEF:    return parse_function_expression();
  case IF:
  case UNLESS: return parse_if_expression();
  case CASE:   return parse_case_expression();
  case WHILE:
  case UNTIL:  return parse_while_expression();
  case FOR:    return parse_for_expression();
  }

  ptr<expression> expression = parse_expression();
  if (current_token().m_type == IF || current_token().m_type == UNLESS)
    return parse_if_expression(expression);
  else if (current_token().m_type == WHILE || current_token().m_type == UNTIL)
    return parse_while_expression(expression);

  assert_token(NEW_LINE);
  return expression;
}

ptr<expression> parser::parse_expression()
{
  if (is_current_token_unary_operator())
  {
    token_type oper = current_token().m_type;
    ++m_position;

    return create_pointer<unary_expression>(oper, parse_expression());
  }

  ptr<expression> left;
  if (current_token().m_type == IDENTIFIER)
    left = parse_call_expression();
  else
    left = parse_literal_expression();

  if (is_current_token_binary_operator())
  {
    token_type oper = current_token().m_type;
    ++m_position;

    return create_pointer<binary_expression>(oper, left, parse_expression());
  }

  return left;
}

ptr<expression> parser::parse_class_expression()
{
  assert_token(CLASS, MODULE);
  ptr<expression> name = parse_identifier_expression();
  assert_token(NEW_LINE);
  ptr<expression> body = parse_program_expression();
  assert_token(END);

  return create_pointer<class_expression>(name, body);
}

ptr<expression> parser::parse_function_expression()
{
  assert_token(DEF);
  ptr<expression> name = parse_identifier_expression();
  assert_token(OPEN_PARENTHESIS);
  ptr<expression> args = parse_arguments_expression();
  assert_token(CLOSED_PARENTHESIS);
  assert_token(NEW_LINE);
  ptr<expression> body = parse_program_expression();
  assert_token(END);

  return create_pointer<function_expression>(name, args, body);
}

ptr<expression> parser::parse_identifier_expression()
{
  string value = assert_token(IDENTIFIER);

  while (current_token().m_type == DOUBLE_COLON)
  {
    ++m_position;

    value += "::";
    value += assert_token(IDENTIFIER);
  }

  while (current_token().m_type == DOT)
  {
    ++m_position;

    value += ".";
    value += assert_token(IDENTIFIER);
  }

  return create_pointer<identifier_expression>(value);
}

ptr<expression> parser::parse_arguments_expression()
{
  vector<ptr<expression>> args;
  while (current_token().m_type == IDENTIFIER)
  {
    args.push_back(parse_identifier_expression());

    if (current_token().m_type == COMMA)
      ++m_position;
  }

  return create_pointer<arguments_expression>(args);
}

ptr<expression> parser::parse_call_expression()
{
  ptr<expression> identifier = parse_identifier_expression();
  ptr<expression> args = nullptr;
  ptr<expression> block = nullptr;

  if (current_token().m_type == OPEN_PARENTHESIS)
  {
    ++m_position;

    args = parse_arguments_expression();
    assert_token(CLOSED_PARENTHESIS);
  }

  if (current_token().m_type == DO)
    block = parse_block_expression();

  return create_pointer<call_expression>(identifier, block);
}

ptr<expression> parser::parse_block_expression()
{
  assert_token(DO);
  ptr<expression> args = nullptr;
  if (current_token().m_type == PIPE)
  {
    ++m_position;

    args = parse_arguments_expression();
    assert_token(PIPE);
  }
  assert_token(NEW_LINE);
  ptr<expression> body = parse_program_expression();
  assert_token(END);

  return create_pointer<function_expression>(nullptr, args, body);
}

ptr<expression> parser::parse_if_expression(ptr<expression> body)
{
  if (current_token().m_type == UNLESS)
  {
    ++m_position;

    ptr<expression> condition = create_pointer<unary_expression>(EXCLAMATION_MARK, parse_expression());
    if (body != nullptr)
      return create_pointer<if_expression>(condition, body);

    assert_token(NEW_LINE);
    ptr<expression> then_expr = parse_program_expression();
    ptr<expression> else_expr = nullptr;
    if (current_token().m_type == ELSE)
    {
      ++m_position;
      else_expr = parse_program_expression();
    }
    assert_token(END);

    return create_pointer<if_expression>(condition, then_expr, else_expr);
  }

  assert_token(IF, ELSIF);
  ptr<expression> condition = parse_expression();
  if (body != nullptr)
    return create_pointer<if_expression>(condition, body);

  assert_token(NEW_LINE);
  ptr<expression> then_expr = parse_program_expression();
  ptr<expression> else_expr = nullptr;
  if (current_token().m_type == ELSIF)
  {
    return create_pointer<if_expression>(condition, then_expr, parse_if_expression());
  }
  else if (current_token().m_type == ELSE)
  {
    ++m_position;
    else_expr = parse_program_expression();
  }
  assert_token(END);

  return create_pointer<if_expression>(condition, then_expr, else_expr);
}

ptr<expression> parser::parse_case_expression()
{
  assert_token(CASE);
  ptr<expression> condition = parse_expression();
  assert_token(NEW_LINE);
  vector<ptr<expression>> when_exprs;

  while (current_token().m_type == WHEN)
    when_exprs.push_back(parse_when_expression());

  ptr<expression> else_expr = nullptr;
  if (current_token().m_type == ELSE)
  {
    ++m_position;
    else_expr = parse_program_expression();
  }
  assert_token(END);

  return create_pointer<case_expression>(condition, when_exprs, else_expr);
}

ptr<expression> parser::parse_when_expression()
{
  assert_token(WHEN);
  ptr<expression> condition = parse_expression();
  assert_token(THEN);

  return create_pointer<when_expression>(condition, parse_program_expression());
}

ptr<expression> parser::parse_while_expression(ptr<expression> body)
{
  if (current_token().m_type == UNTIL)
  {
    ++m_position;

    ptr<expression> condition = create_pointer<unary_expression>(EXCLAMATION_MARK, parse_expression());
    if (body != nullptr)
      return create_pointer<while_expression>(condition, body);

    assert_token(NEW_LINE);
    body = parse_program_expression();
    assert_token(END);

    return create_pointer<while_expression>(condition, body);
  }

  assert_token(WHILE);
  ptr<expression> condition = parse_expression();
  if (body != nullptr)
    return create_pointer<while_expression>(condition, body);

  assert_token(NEW_LINE);
  body = parse_program_expression();
  assert_token(END);

  return create_pointer<while_expression>(condition, body);
}

ptr<expression> parser::parse_for_expression()
{
  assert_token(FOR);
  ptr<expression> var = parse_identifier_expression();
  assert_token(IN);
  ptr<expression> expr = parse_expression();
  assert_token(DO);
  assert_token(NEW_LINE);
  ptr<expression> body = parse_program_expression();
  assert_token(END);

  return create_pointer<for_expression>(var, expr, body);
}

ptr<expression> parser::parse_literal_expression()
{
  switch (current_token().m_type)
  {
  case OPEN_CURLY_BRACE:    return parse_hash_expression();
  case OPEN_SQUARE_BRACKET: return parse_list_expression();
  case SINGLE_QUOTE_STRING:
  case DOUBLE_QUOTE_STRING: return parse_string_expression();
  case SYMBOL:              return parse_symbol_expression();
  case NUMBER:              return parse_number_expression();
  case TRUE:
  case FALSE:               return parse_boolean_expression();
  case NIL:                 return parse_nil_expression();
  }

  assert(false);
}

ptr<expression> parser::parse_hash_expression()
{
  assert_token(OPEN_CURLY_BRACE);
  ptr<hash_expression> hash = create_pointer<hash_expression>();
  while (current_token().m_type != CLOSED_CURLY_BRACE)
  {
    hash->m_elements.push_back(parse_hash_element_expression());

    if (current_token().m_type == COMMA)
      ++m_position;
  }
  assert_token(CLOSED_CURLY_BRACE);

  return hash;
}

ptr<expression> parser::parse_hash_element_expression()
{
  ptr<expression> key = parse_identifier_expression();
  assert_token(EQUAL_GREATER_THAN);
  ptr<expression> value = parse_expression();

  return create_pointer<hash_element_expression>(key, value);
}

ptr<expression> parser::parse_list_expression()
{
  assert_token(OPEN_SQUARE_BRACKET);
  ptr<list_expression> list = create_pointer<list_expression>();
  while (current_token().m_type != CLOSED_SQUARE_BRACKET)
  {
    list->m_elements.push_back(parse_expression());

    if (current_token().m_type == COMMA)
      ++m_position;
  }
  assert_token(CLOSED_SQUARE_BRACKET);

  return list;
}

ptr<expression> parser::parse_string_expression()
{
  string value = assert_token(SINGLE_QUOTE_STRING, DOUBLE_QUOTE_STRING);

  return create_pointer<string_expression>(value);
}

ptr<expression> parser::parse_symbol_expression()
{
  string value = assert_token(SYMBOL);

  return create_pointer<symbol_expression>(value);
}

ptr<expression> parser::parse_number_expression()
{
  string value = assert_token(NUMBER);

  return create_pointer<number_expression>(std::stod(value));
}

ptr<expression> parser::parse_boolean_expression()
{
  string value = assert_token(TRUE, FALSE);

  return create_pointer<boolean_expression>(value == "true" ? true : false);
}

ptr<expression> parser::parse_nil_expression()
{
  assert_token(NIL);

  return create_pointer<nil_expression>();
}

token parser::current_token()
{
  return m_tokens[m_position];
}

string parser::assert_token(token_type type, token_type backup_type)
{
  if (backup_type != UNRECOGNIZED)
    assert(current_token().m_type == type || current_token().m_type == backup_type);
  else
    assert(current_token().m_type == type);

  return m_tokens[m_position++].m_lexeme;
}

bool parser::is_current_token_unary_operator()
{
  token_type type = current_token().m_type;

  return (
    type == EXCLAMATION_MARK ||
    type == NOT              ||
    type == PLUS             ||
    type == MINUS
  );
}

bool parser::is_current_token_binary_operator()
{
  token_type type = current_token().m_type;

  return (
    type == DOUBLE_AMPERSAND       ||
    type == AND                    ||
    type == DOUBLE_PIPE            ||
    type == OR                     ||
    type == LESS_THAN              ||
    type == LESS_THAN_EQUAL        ||
    type == GREATER_THAN           ||
    type == GREATER_THAN_EQUAL     ||
    type == EXCLAMATION_MARK_EQUAL ||
    type == EQUAL                  ||
    type == DOUBLE_EQUAL           ||
    type == PLUS                   ||
    type == PLUS_EQUAL             ||
    type == MINUS                  ||
    type == MINUS_EQUAL            ||
    type == ASTERISK               ||
    type == ASTERISK_EQUAL         ||
    type == DOUBLE_ASTERISK        ||
    type == DOUBLE_ASTERISK_EQUAL  ||
    type == SLASH                  ||
    type == SLASH_EQUAL            ||
    type == PROCENT                ||
    type == PROCENT_EQUAL
  );
}
