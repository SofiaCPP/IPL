#include "parser.hpp"

parser::parser(const vector<token>& tokens) :
  m_tokens(tokens), m_position(0)
{
}

expression_ptr parser::parse_program_expression()
{
  ptr<program_expression> program = create_pointer<program_expression>();

  while (
    m_position < m_tokens.size()  &&
    current_token_type() != END   &&
    current_token_type() != ELSIF &&
    current_token_type() != ELSE  &&
    current_token_type() != WHEN
  ) {
    if (current_token_type() == NEW_LINE)
      ++m_position;
    else
      program->m_statements.push_back(parse_statement());
  }

  LOG(program->m_statements.size());

  return program;
}

expression_ptr parser::parse_statement()
{
  switch (m_tokens[m_position].m_type)
  {
  case IF:
  case UNLESS: return parse_if_expression();
  case CASE:   return parse_case_expression();
  case WHILE:
  case UNTIL:  return parse_while_expression();
  case FOR:    return parse_for_expression();
  case CLASS:
  case MODULE: return parse_class_expression();
  case DEF:    return parse_function_expression();
  }

  expression_ptr expression = parse_expression();
  if (current_token_type() == IF || current_token_type() == UNLESS)
    return parse_if_expression(expression);
  else if (current_token_type() == WHILE || current_token_type() == UNTIL)
    return parse_while_expression(expression);

  assert_next(NEW_LINE);
  return expression;
}

expression_ptr parser::parse_expression()
{
  if (is_current_token_unary_operator())
  {
    token_type oper = current_token_type();
    ++m_position;

    return create_pointer<unary_expression>(oper, parse_expression());
  }

  expression_ptr left;
  if (current_token_type() == IDENTIFIER)
    left = parse_call_expression();
  else
    left = parse_literal_expression();

  if (is_current_token_binary_operator())
  {
    token_type oper = current_token_type();
    ++m_position;

    return create_pointer<binary_expression>(oper, left, parse_expression());
  }

  return left;
}

expression_ptr parser::parse_identifier_expression()
{
  string value = assert_next(IDENTIFIER);

  while (current_token_type() == DOUBLE_COLON)
  {
    assert_next((DOUBLE_COLON));
    value += "::";
    value += assert_next(IDENTIFIER);
  }

  while (current_token_type() == DOT)
  {
    assert_next(DOT);
    value += ".";
    value += assert_next(IDENTIFIER);
  }

  return create_pointer<identifier_expression>(value);
}

expression_ptr parser::parse_if_expression(expression_ptr body)
{
  if (current_token_type() == UNLESS)
  {
    assert_next(UNLESS);

    expression_ptr condition = create_pointer<unary_expression>(EXCLAMATION_MARK, parse_expression());

    if (body != nullptr)
      return create_pointer<if_expression>(condition, body);

    assert_next(NEW_LINE);

    expression_ptr then_expr = parse_program_expression();
    expression_ptr else_expr = nullptr;

    if (current_token_type() == ELSE)
    {
      ++m_position;
      else_expr = parse_program_expression();
    }

    assert_next(END);

    return create_pointer<if_expression>(condition, then_expr, else_expr);
  }

  assert_next(IF, ELSIF);

  expression_ptr condition = parse_expression();
  if (body != nullptr)
    return create_pointer<if_expression>(condition, body);

  assert_next(NEW_LINE);

  expression_ptr then_expr = parse_program_expression();
  expression_ptr else_expr = nullptr;

  if (current_token_type() == ELSIF)
  {
    return create_pointer<if_expression>(condition, then_expr, parse_if_expression());
  }
  else if (current_token_type() == ELSE)
  {
    ++m_position;

    else_expr = parse_program_expression();
  }

  assert_next(END);

  return create_pointer<if_expression>(condition, then_expr, else_expr);
}

expression_ptr parser::parse_case_expression()
{
  assert_next(CASE);

  expression_ptr condition = parse_expression();

  assert_next(NEW_LINE);

  vector<expression_ptr> when_exprs;
  while (current_token_type() == WHEN)
    when_exprs.push_back(parse_when_expression());

  expression_ptr else_expr = nullptr;
  if (current_token_type() == ELSE)
  {
    ++m_position;
    else_expr = parse_program_expression();
  }

  assert_next(END);

  return create_pointer<case_expression>(condition, when_exprs, else_expr);
}

expression_ptr parser::parse_when_expression()
{
  assert_next(WHEN);

  expression_ptr condition = parse_expression();

  assert_next(THEN);

  return create_pointer<when_expression>(condition, parse_program_expression());
}

expression_ptr parser::parse_while_expression(expression_ptr body)
{
  if (current_token_type() == UNTIL)
  {
    assert_next(UNTIL);

    expression_ptr condition = create_pointer<unary_expression>(EXCLAMATION_MARK, parse_expression());

    if (body != nullptr)
      return create_pointer<while_expression>(condition, body);

    assert_next(NEW_LINE);

    body = parse_program_expression();

    assert_next(END);

    return create_pointer<while_expression>(condition, body);
  }

  assert_next(WHILE);

  expression_ptr condition = parse_expression();
  if (body != nullptr)
    return create_pointer<while_expression>(condition, body);

  assert_next(NEW_LINE);

  body = parse_program_expression();

  assert_next(END);

  return create_pointer<while_expression>(condition, body);
}

expression_ptr parser::parse_for_expression()
{
  assert_next(FOR);

  expression_ptr var = parse_identifier_expression();

  assert_next(IN);

  expression_ptr expr = parse_expression();

  assert_next(DO);
  assert_next(NEW_LINE);

  expression_ptr body = parse_program_expression();

  assert_next(END);

  return create_pointer<for_expression>(var, expr, body);
}

expression_ptr parser::parse_class_expression()
{
  assert_next(CLASS, MODULE);

  expression_ptr name = parse_identifier_expression();

  assert_next(NEW_LINE);

  expression_ptr body = parse_program_expression();

  assert_next(END);

  return create_pointer<class_expression>(name, body);
}

expression_ptr parser::parse_function_expression()
{
  assert_next(DEF);

  expression_ptr name = parse_identifier_expression();

  assert_next(OPEN_PARENTHESIS);

  expression_ptr args = parse_arguments_expression();

  assert_next(CLOSED_PARENTHESIS);
  assert_next(NEW_LINE);

  expression_ptr body = parse_program_expression();

  assert_next(END);

  return create_pointer<function_definition>(name, args, body);
}

expression_ptr parser::parse_call_expression()
{
  expression_ptr identifier = parse_identifier_expression();
  expression_ptr args = nullptr;
  expression_ptr block = nullptr;

  if (current_token_type() == OPEN_PARENTHESIS)
  {
    ++m_position;
    args = parse_arguments_expression();

    assert_next(CLOSED_PARENTHESIS);
  }

  if (current_token_type() == DO)
    block = parse_block_expression();

  return create_pointer<call_expression>(identifier, block);
}


expression_ptr parser::parse_arguments_expression()
{
  vector<expression_ptr> args;
  while (current_token_type() == IDENTIFIER)
  {
    args.push_back(parse_identifier_expression());

    if (current_token_type() == COMMA)
      ++m_position;
  }

  return create_pointer<arguments_expression>(args);
}

expression_ptr parser::parse_block_expression()
{
  assert_next(DO);

  expression_ptr args = nullptr;
  if (current_token_type() == PIPE)
  {
    ++m_position;
    args = parse_arguments_expression();

    assert_next(PIPE);
  }

  assert_next(NEW_LINE);

  expression_ptr body = parse_program_expression();

  assert_next(END);

  return create_pointer<function_definition>(nullptr, args, body);
}

expression_ptr parser::parse_literal_expression()
{
  switch (current_token_type())
  {
  case NIL:                 return parse_literal_nil();
  case NUMBER:              return parse_literal_number();
  case SINGLE_QUOTE_STRING:
  case DOUBLE_QUOTE_STRING: return parse_literal_string();
  case SYMBOL:              return parse_literal_symbol();
  case OPEN_SQUARE_BRACKET: return parse_literal_list();
  case OPEN_CURLY_BRACE:    return parse_literal_hash();
  case TRUE:
  case FALSE:               return parse_literal_boolean();
  }

  assert(false);
}

expression_ptr parser::parse_literal_nil()
{
  assert_next(NIL);
  return create_pointer<literal_nil>();
}

expression_ptr parser::parse_literal_boolean()
{
  string value = assert_next(TRUE, FALSE);
  return create_pointer<literal_boolean>(value == "true" ? true : false);
}

expression_ptr parser::parse_literal_number()
{
  string value = assert_next(NUMBER);
  return create_pointer<literal_number>(std::stod(value));
}

expression_ptr parser::parse_literal_string()
{
  string value = assert_next(SINGLE_QUOTE_STRING, DOUBLE_QUOTE_STRING);
  return create_pointer<literal_string>(value);
}

expression_ptr parser::parse_literal_symbol()
{
  string value = assert_next(SYMBOL);
  return create_pointer<literal_symbol>(value);
}

expression_ptr parser::parse_literal_list()
{
  assert_next(OPEN_SQUARE_BRACKET);

  ptr<literal_list> list = create_pointer<literal_list>();
  while (current_token_type() != CLOSED_SQUARE_BRACKET)
  {
    list->m_values.push_back(parse_expression());

    if (current_token_type() == COMMA)
      ++m_position;
  }

  assert_next(CLOSED_SQUARE_BRACKET);

  return list;
}

expression_ptr parser::parse_literal_hash()
{
  assert_next(OPEN_CURLY_BRACE);

  ptr<literal_hash> hash = create_pointer<literal_hash>();
  while (current_token_type() != CLOSED_CURLY_BRACE)
  {
    hash->m_values.push_back(parse_literal_hash_element());

    if (current_token_type() == COMMA)
      ++m_position;
  }

  assert_next(CLOSED_CURLY_BRACE);

  return hash;
}

expression_ptr parser::parse_literal_hash_element()
{
  expression_ptr key = parse_identifier_expression();

  assert_next(EQUAL_GREATER_THAN);

  expression_ptr value = parse_expression();

  return create_pointer<literal_hash_element>(key, value);
}

token_type parser::current_token_type()
{
  return m_tokens[m_position].m_type;
}

string parser::assert_next(token_type type, token_type backup_type)
{
  if (backup_type != UNRECOGNIZED)
    assert(current_token_type() == type || current_token_type() == backup_type);
  else
    assert(current_token_type() == type);

  return m_tokens[m_position++].m_lexeme;
}

bool parser::is_current_token_unary_operator()
{
  return (
    current_token_type() == EXCLAMATION_MARK ||
    current_token_type() == NOT              ||
    current_token_type() == PLUS             ||
    current_token_type() == MINUS
  );
}

bool parser::is_current_token_binary_operator()
{
  return (
    current_token_type() == PLUS                  ||
    current_token_type() == PLUS_EQUAL            ||
    current_token_type() == MINUS                 ||
    current_token_type() == MINUS_EQUAL           ||
    current_token_type() == ASTERISK              ||
    current_token_type() == ASTERISK_EQUAL        ||
    current_token_type() == DOUBLE_ASTERISK       ||
    current_token_type() == DOUBLE_ASTERISK_EQUAL ||
    current_token_type() == DOUBLE_AMPERSAND      ||
    current_token_type() == SLASH                 ||
    current_token_type() == SLASH_EQUAL           ||
    current_token_type() == PROCENT               ||
    current_token_type() == PROCENT_EQUAL         ||
    current_token_type() == AND                   ||
    current_token_type() == DOUBLE_PIPE           ||
    current_token_type() == OR                    ||
    current_token_type() == EQUAL                 ||
    current_token_type() == DOUBLE_EQUAL          ||
    current_token_type() == LESS_THAN             ||
    current_token_type() == LESS_THAN_EQUAL       ||
    current_token_type() == GREATER_THAN          ||
    current_token_type() == GREATER_THAN_EQUAL
  );
}
