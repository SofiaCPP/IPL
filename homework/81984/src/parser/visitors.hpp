#ifndef RUBY_IPL_PARSER_VISITORS_HPP_
#define RUBY_IPL_PARSER_VISITORS_HPP_

#include "nodes.hpp"
#include "../html/syntax_highlighter.hpp"

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

struct DisplayVisitor
{
  SyntaxHighlighter highlighter;
  Index tabulation;

  DisplayVisitor();
  ~DisplayVisitor() = default;

  String Visit(Program* node);
  String Visit(Arguments* node);
  String Visit(FunctionChain* node);
  String Visit(Block* node);
  String Visit(Call* node);
  String Visit(Variable* node);
  String Visit(Function* node);
  String Visit(Condition* node);
  String Visit(Conditions* node);
  String Visit(Conditional* node);
private:
  String Tab();
};

#endif
