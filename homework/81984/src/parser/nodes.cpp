#include "nodes.hpp"

String Node::Accept(DisplayVisitor& visitor)
{
  return "";
}

Node::~Node()
{
}

String Program::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Arguments::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String FunctionChain::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Block::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Call::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Variable::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Function::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Condition::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Conditions::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}

String Conditional::Accept(DisplayVisitor& visitor)
{
  return visitor.Visit(this);
}
