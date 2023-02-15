#ifndef EXPRESSION_EXPRESSION_VISITOR_HPP
#define EXPRESSION_EXPRESSION_VISITOR_HPP

#include "expression_predefine.hpp"

class expression_visitor
{
public:
  virtual ~expression_visitor() {}

  virtual void visit(expression* expression) {};

  virtual void visit(program_expression* expression)      = 0;
  virtual void visit(class_expression* expression)        = 0;
  virtual void visit(function_expression* expression)     = 0;
  virtual void visit(identifier_expression* expression)   = 0;
  virtual void visit(arguments_expression* expression)    = 0;
  virtual void visit(call_expression* expression)         = 0;
  virtual void visit(if_expression* expression)           = 0;
  virtual void visit(case_expression* expression)         = 0;
  virtual void visit(when_expression* expression)         = 0;
  virtual void visit(unary_expression* expression)        = 0;
  virtual void visit(binary_expression* expression)       = 0;
  virtual void visit(while_expression* expression)        = 0;
  virtual void visit(for_expression* expression)          = 0;
  virtual void visit(hash_expression* expression)         = 0;
  virtual void visit(hash_element_expression* expression) = 0;
  virtual void visit(list_expression* expression)         = 0;
  virtual void visit(string_expression* expression)       = 0;
  virtual void visit(symbol_expression* expression)       = 0;
  virtual void visit(number_expression* expression)       = 0;
  virtual void visit(boolean_expression* expression)      = 0;
  virtual void visit(nil_expression* expression)          = 0;
};

#endif
