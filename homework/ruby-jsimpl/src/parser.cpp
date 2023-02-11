#include "parser.hpp"

parser::parser(const vector<token>& tokens) :
  m_tokens(tokens), m_position(0)
{
}

expression_ptr parser::parse_program_expression()
{
  auto program = std::make_shared<program_expression>();
  while (auto statement = parse_statement()) program->m_statements.push_back(statement);
  return program;
}

expression_ptr parser::parse_statement()
{
  if (m_tokens[m_position].m_type == OPEN_PARENTHESIS)
  {
    assert_next(OPEN_PARENTHESIS);
    auto program = parse_program_expression();
    assert_next(CLOSED_PARENTHESIS);

    return program;
  }

  switch (m_tokens[m_position].m_type)
  {
  case IF:
  case UNLESS:              return parse_if_statement();
  case CASE:                return parse_case_statement();
  case WHILE:
  case UNTIL:               return parse_while_statement();
  case FOR:                 return parse_for_statement();
  case CLASS:
  case MODULE:              return parse_class_statement();
  case DEF:                 return parse_function_definition_statement();
  }

  expression_ptr expression = parse_expression();
  if (m_tokens[m_position].m_type == IF || m_tokens[m_position].m_type == UNLESS)
    return parse_if_statement(expression);
  else if (m_tokens[m_position].m_type == WHILE || m_tokens[m_position].m_type == UNTIL)
    return parse_while_statement(expression);

  return expression;
}

expression_ptr parser::parse_expression()
{
  if (is_unary_operator())
  {
    token_type oper = m_tokens[m_position++].m_type;
    return std::make_shared<unary_expression>(oper, parse_expression());
  }

  expression_ptr left;
  if (m_tokens[m_position].m_type == IDENTIFIER)
    left = parse_call_expression();
  else
    left = parse_literal_expression();

  if (m_tokens[m_position].m_type == NEWLINE)
    return left;

  if (is_binary_operator())
  {
    token_type oper = m_tokens[m_position++].m_type;
    return std::make_shared<binary_expression>(oper, left, parse_expression());
  }
}

expression_ptr parser::parse_identifier()
{
  string identifier = assert_next(IDENTIFIER);

  while (m_tokens[m_position].m_type == DOUBLE_COLON)
  {
    identifier += assert_next((DOUBLE_COLON));
    identifier += assert_next(IDENTIFIER);
  }

  while (m_tokens[m_position].m_type == DOT)
  {
    identifier += assert_next(DOT);
    identifier += assert_next(IDENTIFIER);
  }

  return std::make_shared<identifier_expression>(identifier);
}

expression_ptr parser::parse_if_statement(expression_ptr body)
{
  if (m_tokens[m_position].m_type == UNLESS)
  {
    assert_next(UNLESS);
    expression_ptr condition = std::make_shared<unary_expression>(EXCLAMATION_MARK, parse_expression());

    if (body != nullptr)
      return std::make_shared<if_expression>(condition, body);

    assert_next(NEWLINE);
    expression_ptr then_expr = parse_program_expression();
    expression_ptr else_expr = nullptr;

    if (m_tokens[m_position].m_type == ELSE)
    {
      assert_next(ELSE);
      else_expr = parse_program_expression();
    }

    assert_next(END);
    return std::make_shared<if_expression>(condition, then_expr, else_expr);
  }

  assert_next(IF, ELSIF);
  expression_ptr condition = parse_expression();
  if (body != nullptr)
    return std::make_shared<if_expression>(condition, body);

  assert_next(NEWLINE);
  expression_ptr then_expr = parse_program_expression();
  expression_ptr else_expr = nullptr;

  if (m_tokens[m_position].m_type == ELSIF)
  {
    assert_next(ELSIF);
    return std::make_shared<if_expression>(condition, then_expr, parse_if_statement());
  }
  else if (m_tokens[m_position].m_type == ELSE)
  {
    assert_next(ELSE);
    else_expr = parse_program_expression();
  }

  assert_next(END);
  return std::make_shared<if_expression>(condition, then_expr, else_expr);
}

expression_ptr parser::parse_case_statement()
{
  assert_next(CASE);
  expression_ptr condition = parse_expression();
  assert_next(NEWLINE);

  vector<expression_ptr> when_exprs;
  while (m_tokens[m_position].m_type == WHEN)
  {
    when_exprs.push_back(parse_when_expression());
    assert_next(NEWLINE);
  }

  expression_ptr else_expr = nullptr;
  if (m_tokens[m_position].m_type == ELSE)
    else_expr = parse_program_expression();

  assert_next(END);
  return std::make_shared<case_expression>(condition, when_exprs, else_expr);
}

expression_ptr parser::parse_when_expression()
{
  assert_next(WHEN);
  expression_ptr condition = parse_expression();
  assert_next(COLON);
  return std::make_shared<when_expression>(condition, parse_program_expression());
}

expression_ptr parser::parse_while_statement(expression_ptr body)
{
  if (m_tokens[m_position].m_type == UNTIL)
  {
    assert_next(UNTIL);
    expression_ptr condition = std::make_shared<unary_expression>(EXCLAMATION_MARK, parse_expression());

    if (body != nullptr)
      return std::make_shared<while_expression>(condition, body);

    assert_next(NEWLINE);
    body = parse_program_expression();
    assert_next(END);

    return std::make_shared<while_expression>(condition, body);
  }

  assert_next(WHILE);
  expression_ptr condition = parse_expression();
  if (body != nullptr)
    return std::make_shared<while_expression>(condition, body);

  assert_next(NEWLINE);
  body = parse_program_expression();
  assert_next(END);

  return std::make_shared<while_expression>(condition, body);
}

expression_ptr parser::parse_for_statement()
{
  assert_next(FOR);
  expression_ptr variable = parse_identifier();
  assert_next(IN);
  expression_ptr expression = parse_expression();
  assert_next(DO);
  assert_next(NEWLINE);
  expression_ptr body = parse_program_expression();
  assert_next(END);
  return std::make_shared<for_expression>(variable, expression, body);
}

expression_ptr parser::parse_class_statement()
{
  assert_next(CLASS, MODULE);
  expression_ptr identifier = parse_identifier();
  expression_ptr body = parse_program_expression();
  assert_next(END);
  return std::make_shared<class_expression>(identifier, body);
}

expression_ptr parser::parse_function_definition_statement()
{
  assert_next(DEF);
  expression_ptr identifier = parse_identifier();
  vector<expression_ptr> arguments;
  do
  {
    arguments.push_back(parse_identifier());
  }
  while (m_tokens[m_position++].m_type == COMMA);
  assert_next(NEWLINE);
  expression_ptr body = parse_program_expression();
  assert_next(END);
  return std::make_shared<function_definition>(identifier, arguments, body);
}

expression_ptr parser::parse_call_expression()
{
  expression_ptr identifier = parse_identifier();
  expression_ptr block = nullptr;
  if (m_tokens[m_position].m_type == DO)
    block = parse_block();

  return std::make_shared<call_expression>(identifier, block);
}

expression_ptr parser::parse_block()
{
  assert_next(DO);
  if (m_tokens[m_position].m_type == PIPE)
  {
    assert_next(PIPE);
    assert_next(PIPE);
  }
  assert_next(NEWLINE);
  return std::make_shared<function_definition>(nullptr, vector<expression_ptr>(), parse_program_expression());
}

expression_ptr parser::parse_literal_expression()
{
  switch (m_tokens[m_position].m_type)
  {
  case NIL:                 return parse_literal_nil();
  case NUMBER:              return parse_literal_number();
  case SINGLE_QUOTE_STRING:
  case DOUBLE_QUOTE_STRING: return parse_literal_string();
  case SYMBOL:              return parse_literal_symbol();
  case OPEN_SQUARE_BRACKET: return parse_literal_list();
  case OPEN_CURLY_BRACE:    return parse_literal_hash();
  }
}

expression_ptr parser::parse_literal_nil()
{
  assert_next(NIL);
  return std::make_shared<literal_nil>();
}

expression_ptr parser::parse_literal_number()
{
  string number = assert_next(NUMBER);
  return std::make_shared<literal_number>(std::stod(number));
}

expression_ptr parser::parse_literal_string()
{
  string str = assert_next(SINGLE_QUOTE_STRING, DOUBLE_QUOTE_STRING);
  return std::make_shared<literal_string>(str);
}

expression_ptr parser::parse_literal_symbol()
{
  string symbol = assert_next(SYMBOL);
  return std::make_shared<literal_symbol>(symbol);
}

expression_ptr parser::parse_literal_list()
{
  assert_next(OPEN_SQUARE_BRACKET);

  auto list = std::make_shared<literal_list>();
  while (m_tokens[m_position].m_type != CLOSED_SQUARE_BRACKET)
  {
    list->m_values.push_back(parse_expression());

    if (m_tokens[m_position].m_type == COMMA)
      assert_next(COMMA);
  }

  assert_next(CLOSED_SQUARE_BRACKET);

  return list;
}

expression_ptr parser::parse_literal_hash()
{
  assert_next(OPEN_CURLY_BRACE);

  auto hash = std::make_shared<literal_hash>();
  while (m_tokens[m_position].m_type == CLOSED_CURLY_BRACE)
  {
    hash->m_values.push_back(parse_literal_hash_element());

    if (m_tokens[m_position].m_type == COMMA)
      assert_next(COMMA);
  }

  assert_next(CLOSED_SQUARE_BRACKET);

  return hash;
}

expression_ptr parser::parse_literal_hash_element()
{
  assert_next(IDENTIFIER);
  auto key = parse_identifier();
  assert_next(EQUAL_GREATER_THAN);
  auto value = parse_expression();
  return std::make_shared<literal_hash_element>(key, value);
}

string parser::assert_next(token_type type, token_type backup)
{
  if (backup != UNRECOGNIZED)
    assert(m_tokens[m_position].m_type == type || m_tokens[m_position].m_type == backup);
  else
    assert(m_tokens[m_position].m_type == type);

  return m_tokens[m_position].m_lexeme;
}

bool parser::is_unary_operator()
{
  return (
    m_tokens[m_position].m_type == EXCLAMATION_MARK ||
    m_tokens[m_position].m_type == PLUS             ||
    m_tokens[m_position].m_type == MINUS
  );
}

bool parser::is_binary_operator()
{
  return (
    m_tokens[m_position].m_type == PLUS                  ||
    m_tokens[m_position].m_type == PLUS_EQUAL            ||
    m_tokens[m_position].m_type == MINUS                 ||
    m_tokens[m_position].m_type == MINUS_EQUAL           ||
    m_tokens[m_position].m_type == ASTERISK              ||
    m_tokens[m_position].m_type == ASTERISK_EQUAL        ||
    m_tokens[m_position].m_type == DOUBLE_ASTERISK       ||
    m_tokens[m_position].m_type == DOUBLE_ASTERISK_EQUAL ||
    m_tokens[m_position].m_type == DOUBLE_AMPERSAND      ||
    m_tokens[m_position].m_type == SLASH                 ||
    m_tokens[m_position].m_type == SLASH_EQUAL           ||
    m_tokens[m_position].m_type == PROCENT               ||
    m_tokens[m_position].m_type == PROCENT_EQUAL         ||
    m_tokens[m_position].m_type == AND                   ||
    m_tokens[m_position].m_type == DOUBLE_PIPE           ||
    m_tokens[m_position].m_type == OR                    ||
    m_tokens[m_position].m_type == EQUAL                 ||
    m_tokens[m_position].m_type == DOUBLE_EQUAL
  );
}
