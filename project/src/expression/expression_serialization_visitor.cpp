#include "expression_serialization_visitor.hpp"

#include "expression.hpp"

void expression_serialization_visitor::visit(literal_nil* expression)
{
  m_output += "null";
}

void expression_serialization_visitor::visit(literal_boolean* expression)
{
  m_output += expression->m_value == true ? "true" : "false";
}

void expression_serialization_visitor::visit(literal_number* expression)
{
  m_output += std::to_string(expression->m_value);
}

void expression_serialization_visitor::visit(literal_string* expression)
{
  m_output += ('\"' + expression->m_value + '\"');
}

void expression_serialization_visitor::visit(literal_symbol* expression)
{
  m_output += ('\"' + expression->m_value + '\"');
}

void expression_serialization_visitor::visit(literal_list* expression)
{
  m_output += '[';
  for (uint i = 0; i < expression->m_values.size(); ++i)
  {
    expression->m_values[i]->accept(*this);

    if (i < expression->m_values.size() - 1)
      m_output += ',';
  }
  m_output += ']';
}

void expression_serialization_visitor::visit(literal_hash_element* expression)
{
  m_output += "{\"key\":";
  expression->m_key->accept(*this);
  m_output += ",\"value\":";
  expression->m_value->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(literal_hash* expression)
{
  m_output += '[';
  for (uint i = 0; i < expression->m_values.size(); ++i)
  {
    expression->m_values[i]->accept(*this);

    if (i < expression->m_values.size() - 1)
      m_output += ',';
  }
  m_output += ']';
}

void expression_serialization_visitor::visit(identifier_expression* expression)
{
  m_output += ("{\"expression\":\"identifier\",\"value\":\"" + expression->m_value + "\"}");
}

void expression_serialization_visitor::visit(variable_definition* expression)
{
  m_output += "{\"expression\":\"variable\",\"name\":";
  expression->m_name->accept(*this);
  m_output += ",\"value\":";
  expression->m_value->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(function_definition* expression)
{
  m_output += "{\"expression\":\"function\",\"name\":";

  if (expression->m_name != nullptr)
    expression->m_name->accept(*this);
  else
    m_output += "\"anonymous\"";

  m_output += ",\"args\":";
  if (expression->m_args != nullptr)
    expression->m_args->accept(*this);
  else
    m_output += "[]";

  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(unary_expression* expression)
{
  m_output += "{\"expression\":\"unary\",\"operator\":";
  visit_token(expression->m_oper);
  m_output += ",\"expr\":";
  expression->m_expr->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(binary_expression* expression)
{
  m_output += "{\"expression\":\"binary\",\"operator\":";
  visit_token(expression->m_oper);
  m_output += ",\"left\":";
  expression->m_left->accept(*this);
  m_output += ",\"right\":";
  expression->m_right->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(if_expression* expression)
{
  m_output += "{\"expression\":\"if\",\"condition\":";
  expression->m_condition->accept(*this);
  m_output += ",\"then_expr\":";
  expression->m_then_expr->accept(*this);

  if (expression->m_else_expr != nullptr)
  {
    m_output += ",\"else_expr\":";
    expression->m_else_expr->accept(*this);
  }

  m_output += "}";
}

void expression_serialization_visitor::visit(case_expression* expression)
{
  m_output += "{\"expression\":\"case\",\"condition\":";
  expression->m_condition->accept(*this);

  m_output += ",\"when_exprs\":[";
  for (uint i = 0; i < expression->m_when_exprs.size(); ++i)
  {
    expression->m_when_exprs[i]->accept(*this);

    if (i < expression->m_when_exprs.size() - 1)
      m_output += ',';
  }
  m_output += "]";

  if (expression->m_else_expr != nullptr)
  {
    m_output += ",\"else_expr\":";
    expression->m_else_expr->accept(*this);
  }

  m_output += "}";
}

void expression_serialization_visitor::visit(when_expression* expression)
{
  m_output += "{\"expression\":\"when\",\"condition\":";
  expression->m_condition->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(while_expression* expression)
{
  m_output += "{\"expression\":\"while\",\"condition\":";
  expression->m_condition->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(for_expression* expression)
{
  m_output += "{\"expression\":\"for\",\"variable\":";
  expression->m_variable->accept(*this);
  m_output += ",\"expr\":";
  expression->m_expr->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(call_expression* expression)
{
  m_output += "{\"expression\":\"call\",\"caller\":";
  expression->m_caller->accept(*this);

  if (expression->m_block != nullptr)
  {
    m_output += ",\"block\":";
    expression->m_block->accept(*this);
  }

  m_output += "}";
}

void expression_serialization_visitor::visit(arguments_expression* expression)
{
  m_output += "{\"expression\":\"arguments\",\"args\":[";
  for (uint i = 0; i < expression->m_args.size(); ++i)
  {
    expression->m_args[i]->accept(*this);

    if (i < expression->m_args.size() - 1)
      m_output += ',';
  }
  m_output += "]}";
}


void expression_serialization_visitor::visit(class_expression* expression)
{
  m_output += "{\"expression\":\"class\",\"name\":";
  expression->m_identifier->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(program_expression* expression)
{
  LOG(expression->m_statements.size());
  m_output += "[";
  for (uint i = 0; i < expression->m_statements.size(); ++i)
  {
    expression->m_statements[i]->accept(*this);

    if (i < expression->m_statements.size() - 1)
      m_output += ',';
  }
  m_output += "]";
}

void expression_serialization_visitor::to_json(const string& name)
{
  write_file(name, m_output);
}

void expression_serialization_visitor::visit_token(token_type type)
{
  switch (type)
  {
  default:
    m_output += "\"SOME_OPERATOR\"";
    break;
  }
}
