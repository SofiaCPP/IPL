#ifndef RUBY_IPL_PARSER_NODES_HPP_
#define RUBY_IPL_PARSER_NODES_HPP_

#include "../utils/types.hpp"
#include "visitors.hpp"

struct Node
{
  virtual void Accept(Visitor* visitor) = 0;
  virtual ~Node() = 0;
};

struct Program : Node
{
  List<Node*> nodes;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Program() override;
};

struct Arguments : Node
{
  List<String> variables;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Arguments() override = default;
};

struct FunctionChain : Node
{
  List<String> identifier;
  List<Arguments> arguments;

  virtual void Accept(Visitor* visitor) override;
  virtual ~FunctionChain() override = default;
};

struct Block : Node
{
  Arguments arguments;
  Program body;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Block() override = default;
};

struct Call : Node
{
  String caller;
  FunctionChain fchain;
  Block block;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Call() override = default;
};

struct Variable : Node
{
  String identifier;
  String value;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Variable() override = default;
};

struct Function : Block
{
  String identifier;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Function() override = default;
};

struct Condition : Node
{
  String lhs;
  String rhs;
  String op;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Condition() override = default;
};

struct Conditions : Node
{
  List<Condition> conditions;
  List<String> lchain;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Conditions() override = default;
};

struct Conditional : Node
{
  Conditions conditions;
  Program body;
  List<Conditional> aconditions;

  virtual void Accept(Visitor* visitor) override;
  virtual ~Conditional() override = default;
};

#endif
