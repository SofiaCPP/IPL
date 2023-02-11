#ifndef EXPRESSION_EXPRESSION_VISITOR_HPP
#define EXPRESSION_EXPRESSION_VISITOR_HPP

class expression;
class literal_nil;
class literal_boolean;
class literal_number;
class literal_string;
class literal_symbol;
class literal_list;
class literal_hash_element;
class literal_hash;
class identifier_expression;
class variable_definition;
class function_definition;
class unary_expression;
class binary_expression;
class if_expression;
class case_expression;
class when_expression;
class while_expression;
class for_expression;
class call_expression;
class class_expression;
class program_expression;

class expression_visitor
{
public:
  virtual ~expression_visitor() {}

  virtual void visit(expression* expression) {};

  virtual void visit(literal_nil* expression) = 0;
  virtual void visit(literal_boolean* expression) = 0;
  virtual void visit(literal_number* expression) = 0;
  virtual void visit(literal_string* expression) = 0;
  virtual void visit(literal_symbol* expression) = 0;
  virtual void visit(literal_list* expression) = 0;
  virtual void visit(literal_hash_element* expression) = 0;
  virtual void visit(literal_hash* expression) = 0;
  virtual void visit(identifier_expression* expression) = 0;
  virtual void visit(variable_definition* expression) = 0;
  virtual void visit(function_definition* expression) = 0;
  virtual void visit(unary_expression* expression) = 0;
  virtual void visit(binary_expression* expression) = 0;
  virtual void visit(if_expression* expression) = 0;
  virtual void visit(case_expression* expression) = 0;
  virtual void visit(when_expression* expression) = 0;
  virtual void visit(while_expression* expression) = 0;
  virtual void visit(for_expression* expression) = 0;
  virtual void visit(call_expression* expression) = 0;
  virtual void visit(class_expression* expression) = 0;
  virtual void visit(program_expression* expression) = 0;
};

#endif
