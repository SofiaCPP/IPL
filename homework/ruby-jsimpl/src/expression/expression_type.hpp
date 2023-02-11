#ifndef EXPRESSION_EXPRESSION_TYPE_HPP
#define EXPRESSION_EXPRESSION_TYPE_HPP

#include "../types.hpp"
#include "../token/token_type.hpp"

#define NO_MEMBERS(MEMBERS_ITERATOR)

#define LITERAL_BOOLEAN_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(bool, value)

#define LITERAL_NUMBER_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(double, value)

#define LITERAL_STRING_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(string, value)

#define LITERAL_SYMBOL_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(string, value)

#define LITERAL_LIST_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(vector<expression_ptr>, values)

#define LITERAL_HASH_ELEMENT_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, key)                \
  MEMBERS_ITERATOR(expression_ptr, value)

#define LITERAL_HASH_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(vector<expression_ptr>, values)

#define IDENTIFIER_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(string, value)

#define VARIABLE_DEFINITION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, identifier)        \
  MEMBERS_ITERATOR(expression_ptr, value)

#define FUNCTION_DEFINITION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, identifier)        \
  MEMBERS_ITERATOR(vector<expression_ptr>, arguments) \
  MEMBERS_ITERATOR(expression_ptr, body)

#define UNARY_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(token_type, oper)               \
  MEMBERS_ITERATOR(expression_ptr, expr)

#define BINARY_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(token_type, oper)                \
  MEMBERS_ITERATOR(expression_ptr, left)            \
  MEMBERS_ITERATOR(expression_ptr, right)

#define IF_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, condition)   \
  MEMBERS_ITERATOR(expression_ptr, then_expr)   \
  MEMBERS_ITERATOR(expression_ptr, else_expr)

#define CASE_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)      \
  MEMBERS_ITERATOR(expression_ptr, condition)          \
  MEMBERS_ITERATOR(vector<expression_ptr>, when_exprs) \
  MEMBERS_ITERATOR(expression_ptr, else_expr)

#define WHEN_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, condition)     \
  MEMBERS_ITERATOR(expression_ptr, body)

#define WHILE_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, condition)      \
  MEMBERS_ITERATOR(expression_ptr, body)

#define FOR_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, variable)     \
  MEMBERS_ITERATOR(expression_ptr, expr)         \
  MEMBERS_ITERATOR(expression_ptr, body)

#define CALL_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, caller)        \
  MEMBERS_ITERATOR(expression_ptr, block)

#define RETURN_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, expr)

#define CLASS_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(expression_ptr, identifier)     \
  MEMBERS_ITERATOR(expression_ptr, body)

#define PROGRAM_EXPRESSION_MEMBERS(MEMBERS_ITERATOR) \
  MEMBERS_ITERATOR(vector<expression_ptr>, statements)

#define CREATE_EXPRESSIONS(CREATE_EXPRESSION_CLASS)                                          \
  CREATE_EXPRESSION_CLASS(literal_nil, NO_MEMBERS)                                    \
  CREATE_EXPRESSION_CLASS(literal_boolean, LITERAL_BOOLEAN_MEMBERS)                   \
  CREATE_EXPRESSION_CLASS(literal_number, LITERAL_NUMBER_MEMBERS)                     \
  CREATE_EXPRESSION_CLASS(literal_string, LITERAL_STRING_MEMBERS)                     \
  CREATE_EXPRESSION_CLASS(literal_symbol, LITERAL_SYMBOL_MEMBERS)                     \
  CREATE_EXPRESSION_CLASS(literal_list, LITERAL_LIST_MEMBERS)                        \
  CREATE_EXPRESSION_CLASS(literal_hash_element, LITERAL_HASH_ELEMENT_MEMBERS)         \
  CREATE_EXPRESSION_CLASS(literal_hash, LITERAL_HASH_MEMBERS)                         \
  CREATE_EXPRESSION_CLASS(identifier_expression, IDENTIFIER_MEMBERS)                  \
  CREATE_EXPRESSION_CLASS(variable_definition, VARIABLE_DEFINITION_MEMBERS)           \
  CREATE_EXPRESSION_CLASS(function_definition, FUNCTION_DEFINITION_MEMBERS)           \
  CREATE_EXPRESSION_CLASS(unary_expression, UNARY_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(binary_expression, BINARY_EXPRESSION_MEMBERS)               \
  CREATE_EXPRESSION_CLASS(if_expression, IF_EXPRESSION_MEMBERS)                       \
  CREATE_EXPRESSION_CLASS(case_expression, CASE_EXPRESSION_MEMBERS)                   \
  CREATE_EXPRESSION_CLASS(when_expression, WHEN_EXPRESSION_MEMBERS)                   \
  CREATE_EXPRESSION_CLASS(while_expression, WHILE_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(for_expression, FOR_EXPRESSION_MEMBERS)                     \
  CREATE_EXPRESSION_CLASS(call_expression, CALL_EXPRESSION_MEMBERS)                   \
  CREATE_EXPRESSION_CLASS(return_expression, RETURN_EXPRESSION_MEMBERS)               \
  CREATE_EXPRESSION_CLASS(class_expression, CLASS_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(program_expression, PROGRAM_EXPRESSION_MEMBERS)

#endif
