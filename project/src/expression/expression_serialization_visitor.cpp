#include "expression_serialization_visitor.hpp"
#include "expression.hpp"

void expression_serialization_visitor::visit(program_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"program_expression\"";
  m_output += ",\"statements\":";
  visit_list(expression->m_statements);
  m_output += "}";
}

void expression_serialization_visitor::visit(class_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"class_expression\"";
  m_output += ",\"name\":";
  expression->m_name->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(function_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"function_expression\"";

  if (expression->m_name != nullptr)
  {
    m_output += ",\"name\":";
    expression->m_name->accept(*this);
  }

  if (expression->m_args != nullptr)
  {
    m_output += ",\"args\":";
    expression->m_args->accept(*this);
  }

  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(identifier_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"identifier_expression\"";
  m_output += ",\"value\":";
  m_output += "\"" + expression->m_value + "\"";
  m_output += "}";
}

void expression_serialization_visitor::visit(arguments_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"arguments_expression\"";
  m_output += ",\"args\":";
  visit_list(expression->m_args);
  m_output += "}";
}

void expression_serialization_visitor::visit(call_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"call_expression\"";
  m_output += ",\"caller\":";
  expression->m_caller->accept(*this);
  if (expression->m_block != nullptr)
  {
    m_output += ",\"block\":";
    expression->m_block->accept(*this);
  }
  m_output += "}";
}

void expression_serialization_visitor::visit(if_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"if_expression\"";
  m_output += ",\"condition\":";
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
  m_output += "{";
  m_output += "\"type\":\"case_expression\"";
  m_output += ",\"condition\":";
  expression->m_condition->accept(*this);
  m_output += ",\"when_exprs\":";
  visit_list(expression->m_when_exprs);
  if (expression->m_else_expr != nullptr)
  {
    m_output += ",\"else_expr\":";
    expression->m_else_expr->accept(*this);
  }
  m_output += "}";
}

void expression_serialization_visitor::visit(when_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"when_expression\"";
  m_output += ",\"condition\":";
  expression->m_condition->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(unary_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"unary_expression\"";
  m_output += ",\"operator\":";
  visit_token(expression->m_oper);
  m_output += ",\"expr\":";
  expression->m_expr->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(binary_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"binary_expression\"";
  m_output += ",\"operator\":";
  visit_token(expression->m_oper);
  m_output += ",\"left\":";
  expression->m_left->accept(*this);
  m_output += ",\"right\":";
  expression->m_right->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(while_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"while_expression\"";
  m_output += ",\"condition\":";
  expression->m_condition->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(for_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"for_expression\"";
  m_output += ",\"var\":";
  expression->m_var->accept(*this);
  m_output += ",\"expr\":";
  expression->m_expr->accept(*this);
  m_output += ",\"body\":";
  expression->m_body->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(hash_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"hash_expression\"";
  m_output += ",\"elements\":";
  visit_list(expression->m_elements);
  m_output += "}";
}

void expression_serialization_visitor::visit(hash_element_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"hash_element_expression\"";
  m_output += ",\"key\":";
  expression->m_key->accept(*this);
  m_output += ",\"value\":";
  expression->m_value->accept(*this);
  m_output += "}";
}

void expression_serialization_visitor::visit(list_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"list_expression\"";
  m_output += ",\"elements\":";
  visit_list(expression->m_elements);
  m_output += "}";
}

void expression_serialization_visitor::visit(string_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"string_expression\"";
  m_output += ",\"value\":";
  m_output += "\"" + expression->m_value + "\"";
  m_output += "}";
}

void expression_serialization_visitor::visit(symbol_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"symbol_expression\"";
  m_output += ",\"value\":";
  m_output += "\"" + expression->m_value + "\"";
  m_output += "}";
}

void expression_serialization_visitor::visit(number_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"number_expression\"";
  m_output += ",\"value\":";
  m_output += "\"" + std::to_string(expression->m_value) + "\"";
  m_output += "}";
}

void expression_serialization_visitor::visit(boolean_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"boolean_expression\"";
  m_output += ",\"value\":";
  m_output += "\"" + (expression->m_value ? string("true") : string("false")) + "\"";
  m_output += "}";
}

void expression_serialization_visitor::visit(nil_expression* expression)
{
  m_output += "{";
  m_output += "\"type\":\"nil_expression\"";
  m_output += ",\"value\":";
  m_output += "\"" + string("null") + "\"";
  m_output += "}";
}

void expression_serialization_visitor::output(const string& name)
{
  write_file(name, m_output);
}

void expression_serialization_visitor::visit_token(token_type type)
{
  switch (type)
  {
  case EXCLAMATION_MARK:       m_output += "\"!\"";   break;
  case NOT:                    m_output += "\"not\""; break;
  case DOUBLE_AMPERSAND:       m_output += "\"&&\"";  break;
  case AND:                    m_output += "\"and\""; break;
  case DOUBLE_PIPE:            m_output += "\"||\"";  break;
  case OR:                     m_output += "\"or\"";  break;
  case LESS_THAN:              m_output += "\"<\"";   break;
  case LESS_THAN_EQUAL:        m_output += "\"<=\"";  break;
  case GREATER_THAN:           m_output += "\">\"";   break;
  case GREATER_THAN_EQUAL:     m_output += "\">=\"";  break;
  case EXCLAMATION_MARK_EQUAL: m_output += "\"!=\"";  break;
  case EQUAL:                  m_output += "\"=\"";   break;
  case DOUBLE_EQUAL:           m_output += "\"==\"";  break;
  case PLUS:                   m_output += "\"+\"";   break;
  case PLUS_EQUAL:             m_output += "\"+=\"";  break;
  case MINUS:                  m_output += "\"-\"";   break;
  case MINUS_EQUAL:            m_output += "\"-=\"";  break;
  case ASTERISK:               m_output += "\"*\"";   break;
  case ASTERISK_EQUAL:         m_output += "\"*=\"";  break;
  case DOUBLE_ASTERISK:        m_output += "\"**\"";  break;
  case DOUBLE_ASTERISK_EQUAL:  m_output += "\"**=\""; break;
  case SLASH:                  m_output += "\"/\"";   break;
  case SLASH_EQUAL:            m_output += "\"/=\"";  break;
  case PROCENT:                m_output += "\"%\"";   break;
  case PROCENT_EQUAL:          m_output += "\"%=\"";  break;
  }
}

void expression_serialization_visitor::visit_list(vector<ptr<expression>> list)
{
  m_output += '[';
  for (uint i = 0; i < list.size(); ++i)
  {
    list[i]->accept(*this);

    if (i < list.size() - 1)
      m_output += ',';
  }
  m_output += ']';
}
