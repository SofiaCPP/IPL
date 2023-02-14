#ifndef EPRESSION_EXPRESSION_SERIALIZATION_VISITOR_HPP
#define EPRESSION_EXPRESSION_SERIALIZATION_VISITOR_HPP

#include "../types.hpp"
#include "expression_visitor.hpp"

class expression_serialization_visitor : public expression_visitor
{
private:
  string m_output;
  uint m_indent;
public:
  virtual void visit(literal_nil* expression);
  virtual void visit(literal_boolean* expression);
  virtual void visit(literal_number* expression);
  virtual void visit(literal_string* expression);
  virtual void visit(literal_symbol* expression);
  virtual void visit(literal_list* expression);
  virtual void visit(literal_hash_element* expression);
  virtual void visit(literal_hash* expression);
  virtual void visit(identifier_expression* expression);
  virtual void visit(variable_definition* expression);
  virtual void visit(function_definition* expression);
  virtual void visit(unary_expression* expression);
  virtual void visit(binary_expression* expression);
  virtual void visit(if_expression* expression);
  virtual void visit(case_expression* expression);
  virtual void visit(when_expression* expression);
  virtual void visit(while_expression* expression);
  virtual void visit(for_expression* expression);
  virtual void visit(call_expression* expression);
  virtual void visit(class_expression* expression);
  virtual void visit(program_expression* expression);
};

#endif
