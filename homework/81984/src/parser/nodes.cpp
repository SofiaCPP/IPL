#include "nodes.hpp"

void Node::Accept(Visitor* visitor)
{
}

Node::~Node()
{
}

void Program::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Arguments::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void FunctionChain::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Block::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Call::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Variable::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Function::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Condition::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Conditions::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}

void Conditional::Accept(Visitor* visitor)
{
  visitor->Visit(this);
}
