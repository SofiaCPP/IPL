#include "expression_serialization_visitor.hpp"

#include "expression.hpp"

void visit(literal_nil* expression);
void visit(literal_boolean* expression);
void visit(literal_number* expression);
void visit(literal_string* expression);
void visit(literal_symbol* expression);
void visit(literal_list* expression);
void visit(literal_hash_element* expression);
void visit(literal_hash* expression);
void visit(identifier_expression* expression);
void visit(variable_definition* expression);
void visit(function_definition* expression);
void visit(unary_expression* expression);
void visit(binary_expression* expression);
void visit(if_expression* expression);
void visit(case_expression* expression);
void visit(when_expression* expression);
void visit(while_expression* expression);
void visit(for_expression* expression);
void visit(call_expression* expression);
void visit(class_expression* expression);
void visit(program_expression* expression);
