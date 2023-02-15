#ifndef EPRESSION_EXPRESSION_SERIALIZATION_VISITOR_HPP
#define EPRESSION_EXPRESSION_SERIALIZATION_VISITOR_HPP

#include "../types.hpp"
#include "../token/token_type.hpp"
#include "expression_visitor.hpp"

class expression_serialization_visitor : public expression_visitor
{
private:
  string m_output;
public:
  virtual void visit(program_expression* expression);
  virtual void visit(class_expression* expression);
  virtual void visit(function_expression* expression);
  virtual void visit(identifier_expression* expression);
  virtual void visit(arguments_expression* expression);
  virtual void visit(call_expression* expression);
  virtual void visit(if_expression* expression);
  virtual void visit(case_expression* expression);
  virtual void visit(when_expression* expression);
  virtual void visit(unary_expression* expression);
  virtual void visit(binary_expression* expression);
  virtual void visit(while_expression* expression);
  virtual void visit(for_expression* expression);
  virtual void visit(hash_expression* expression);
  virtual void visit(hash_element_expression* expression);
  virtual void visit(list_expression* expression);
  virtual void visit(string_expression* expression);
  virtual void visit(symbol_expression* expression);
  virtual void visit(number_expression* expression);
  virtual void visit(boolean_expression* expression);
  virtual void visit(nil_expression* expression);

  void output(const string& name);
private:
  void visit_list(vector<ptr<expression>> list);
  void visit_token(token_type type);
};

#endif
