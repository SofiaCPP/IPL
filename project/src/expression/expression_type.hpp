#ifndef EXPRESSION_EXPRESSION_TYPE_HPP
#define EXPRESSION_EXPRESSION_TYPE_HPP

#include "../types.hpp"
#include "../token/token_type.hpp"

#define PROGRAM_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                \
  MEMBERS_ITERATOR(vector<ptr<expression>>, statements, vector<ptr<expression>>())

#define CLASS_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                  \
  MEMBERS_ITERATOR(ptr<expression>, name, nullptr)                                  \
  MEMBERS_ITERATOR(ptr<expression>, body, nullptr)

#define FUNCTION_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                               \
  MEMBERS_ITERATOR(ptr<expression>, name, nullptr)                                  \
  MEMBERS_ITERATOR(ptr<expression>, args, nullptr)                                  \
  MEMBERS_ITERATOR(ptr<expression>, body, nullptr)

#define IDENTIFIER_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                             \
  MEMBERS_ITERATOR(string, value, "")

#define ARGUMENTS_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                              \
  MEMBERS_ITERATOR(vector<ptr<expression>>, args, vector<ptr<expression>>())

#define CALL_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                   \
  MEMBERS_ITERATOR(ptr<expression>, caller, nullptr)                                \
  MEMBERS_ITERATOR(ptr<expression>, block, nullptr)

#define IF_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                     \
  MEMBERS_ITERATOR(ptr<expression>, condition, nullptr)                             \
  MEMBERS_ITERATOR(ptr<expression>, then_expr, nullptr)                             \
  MEMBERS_ITERATOR(ptr<expression>, else_expr, nullptr)

#define CASE_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                   \
  MEMBERS_ITERATOR(ptr<expression>, condition, nullptr)                             \
  MEMBERS_ITERATOR(vector<ptr<expression>>, when_exprs, vector<ptr<expression>>())  \
  MEMBERS_ITERATOR(ptr<expression>, else_expr, nullptr)

#define WHEN_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                   \
  MEMBERS_ITERATOR(ptr<expression>, condition, nullptr)                             \
  MEMBERS_ITERATOR(ptr<expression>, body, nullptr)

#define UNARY_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                  \
  MEMBERS_ITERATOR(token_type, oper, UNRECOGNIZED)                                  \
  MEMBERS_ITERATOR(ptr<expression>, expr, nullptr)

#define BINARY_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                 \
  MEMBERS_ITERATOR(token_type, oper, UNRECOGNIZED)                                  \
  MEMBERS_ITERATOR(ptr<expression>, left, nullptr)                                  \
  MEMBERS_ITERATOR(ptr<expression>, right, nullptr)

#define WHILE_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                  \
  MEMBERS_ITERATOR(ptr<expression>, condition, nullptr)                             \
  MEMBERS_ITERATOR(ptr<expression>, body, nullptr)

#define FOR_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                    \
  MEMBERS_ITERATOR(ptr<expression>, var, nullptr)                                   \
  MEMBERS_ITERATOR(ptr<expression>, expr, nullptr)                                  \
  MEMBERS_ITERATOR(ptr<expression>, body, nullptr)

#define HASH_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                   \
  MEMBERS_ITERATOR(vector<ptr<expression>>, elements, vector<ptr<expression>>())

#define HASH_ELEMENT_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                           \
  MEMBERS_ITERATOR(ptr<expression>, key, nullptr)                                   \
  MEMBERS_ITERATOR(ptr<expression>, value, nullptr)

#define LIST_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                   \
  MEMBERS_ITERATOR(vector<ptr<expression>>, elements, vector<ptr<expression>>())

#define STRING_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                 \
  MEMBERS_ITERATOR(string, value, "")

#define SYMBOL_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                 \
  MEMBERS_ITERATOR(string, value, "")

#define NUMBER_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                 \
  MEMBERS_ITERATOR(double, value, 0)

#define BOOLEAN_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)                                \
  MEMBERS_ITERATOR(bool, value, false)

#define NIL_EXPRESSION_MEMBERS(MEMBERS_ITERATOR)

#define CREATE_EXPRESSIONS(CREATE_EXPRESSION_CLASS)                                 \
  CREATE_EXPRESSION_CLASS(program_expression, PROGRAM_EXPRESSION_MEMBERS)           \
  CREATE_EXPRESSION_CLASS(class_expression, CLASS_EXPRESSION_MEMBERS)               \
  CREATE_EXPRESSION_CLASS(function_expression, FUNCTION_EXPRESSION_MEMBERS)         \
  CREATE_EXPRESSION_CLASS(identifier_expression, IDENTIFIER_EXPRESSION_MEMBERS)     \
  CREATE_EXPRESSION_CLASS(arguments_expression, ARGUMENTS_EXPRESSION_MEMBERS)       \
  CREATE_EXPRESSION_CLASS(call_expression, CALL_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(if_expression, IF_EXPRESSION_MEMBERS)                     \
  CREATE_EXPRESSION_CLASS(case_expression, CASE_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(when_expression, WHEN_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(unary_expression, UNARY_EXPRESSION_MEMBERS)               \
  CREATE_EXPRESSION_CLASS(binary_expression, BINARY_EXPRESSION_MEMBERS)             \
  CREATE_EXPRESSION_CLASS(while_expression, WHILE_EXPRESSION_MEMBERS)               \
  CREATE_EXPRESSION_CLASS(for_expression, FOR_EXPRESSION_MEMBERS)                   \
  CREATE_EXPRESSION_CLASS(hash_expression, HASH_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(hash_element_expression, HASH_ELEMENT_EXPRESSION_MEMBERS) \
  CREATE_EXPRESSION_CLASS(list_expression, LIST_EXPRESSION_MEMBERS)                 \
  CREATE_EXPRESSION_CLASS(string_expression, STRING_EXPRESSION_MEMBERS)             \
  CREATE_EXPRESSION_CLASS(symbol_expression, SYMBOL_EXPRESSION_MEMBERS)             \
  CREATE_EXPRESSION_CLASS(number_expression, NUMBER_EXPRESSION_MEMBERS)             \
  CREATE_EXPRESSION_CLASS(boolean_expression, BOOLEAN_EXPRESSION_MEMBERS)           \
  CREATE_EXPRESSION_CLASS(nil_expression, NIL_EXPRESSION_MEMBERS)

#endif
