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
  for (expression_ptr value : expression->m_values)
  {
    value->accept(*this);
    m_output += ',';
  }
  m_output += ']';
}

void expression_serialization_visitor::visit(literal_hash_element* expression)
{
  expression->m_key->accept(*this);
  m_output += ':';
  expression->m_value->accept(*this);
}

void expression_serialization_visitor::visit(literal_hash* expression)
{
  m_output += '{';
  for (expression_ptr value : expression->m_values)
  {
    value->accept(*this);
    m_output += ',';
  }
  m_output += '}';
}

void expression_serialization_visitor::visit(identifier_expression* expression)
{
  m_output += ("{\"expression\":\"identifier\",\"value\":\"" + expression->m_value + "\"}");
}

void expression_serialization_visitor::visit(variable_definition* expression)
{
  m_output += "{\"expression\":\"variable_definition\",\"name\":";
  expression->m_name->accept(*this);
  m_output += ",\"value\":";
  expression->m_value->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(function_definition* expression)
{
  m_output += "{\"expression\":\"function_definition\",\"name\":";
  expression->m_name->accept(*this);
  m_output += ",\"args\":";
  expression->m_args->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(unary_expression* expression)
{
  // NEED OPERATOR TO STRING
}

void expression_serialization_visitor::visit(binary_expression* expression)
{
  // NEED OPERATOR TO STRING
}

void expression_serialization_visitor::visit(if_expression* expression)
{
  // ...
}

void expression_serialization_visitor::visit(case_expression* expression)
{
  // ...
}

void expression_serialization_visitor::visit(when_expression* expression)
{
  // ...
}

void expression_serialization_visitor::visit(while_expression* expression)
{
  // ...
}

void expression_serialization_visitor::visit(for_expression* expression)
{
  // ...
}

void expression_serialization_visitor::visit(call_expression* expression)
{
  // ...
}

void expression_serialization_visitor::visit(class_expression* expression)
{
}

void expression_serialization_visitor::visit(program_expression* expression)
{
}
