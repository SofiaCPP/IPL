#ifndef RUBY_IPL_PARSER_VISITORS_HPP_
#define RUBY_IPL_PARSER_VISITORS_HPP_

#include "nodes.hpp"

struct Program;
struct Arguments;
struct FunctionChain;
struct Block;
struct Call;
struct Variable;
struct Function;
struct Condition;
struct Conditions;
struct Conditional;

struct Visitor
{
  virtual ~Visitor() = 0;

  virtual void Visit(Program* node) = 0;
  virtual void Visit(Arguments* node) = 0;
  virtual void Visit(FunctionChain* node) = 0;
  virtual void Visit(Block* node) = 0;
  virtual void Visit(Call* node) = 0;
  virtual void Visit(Variable* node) = 0;
  virtual void Visit(Function* node) = 0;
  virtual void Visit(Condition* node) = 0;
  virtual void Visit(Conditions* node) = 0;
  virtual void Visit(Conditional* node) = 0;
};

#endif
