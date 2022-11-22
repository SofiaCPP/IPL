#ifndef RUBY_IPL_PARSER_NODES_HPP_
#define RUBY_IPL_PARSER_NODES_HPP_

#include "../utils/types.hpp"
#include "visitors.hpp"

struct DisplayVisitor;

struct Node
{
  virtual String Accept(DisplayVisitor& visitor) = 0;
  virtual ~Node() = 0;
};

struct Program : Node
{
  List<SharedPtr<Node>> nodes;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Program() override = default;
};

struct Arguments : Node
{
  Vector<String> variables;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Arguments() override = default;
};

struct FunctionChain : Node
{
  Vector<String> identifiers;
  Vector<Arguments> arguments;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~FunctionChain() override = default;
};

struct Block : Node
{
  Arguments arguments;
  Program body;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Block() override = default;
};

struct Call : Node
{
  String caller;
  FunctionChain fchain;
  Block block;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Call() override = default;
};

struct Variable : Node
{
  String identifier;
  String value;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Variable() override = default;
};

struct Function : Block
{
  String identifier;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Function() override = default;
};

struct Condition : Node
{
  String lhs;
  String rhs;
  String op;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Condition() override = default;
};

struct Conditions : Node
{
  Vector<Condition> conditions;
  Vector<String> lchain;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Conditions() override = default;
};

struct Conditional : Node
{
  Conditions conditions;
  Program body;
  Vector<Conditional> aconditions;

  virtual String Accept(DisplayVisitor& visitor) override;
  virtual ~Conditional() override = default;
};

#endif
