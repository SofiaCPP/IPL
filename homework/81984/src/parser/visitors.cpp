#include "visitors.hpp"

Visitor::~Visitor()
{
}

void* Visitor::Visit(Program* node)
{
  return (void*)0;
}

void* Visitor::Visit(Arguments* node)
{
  return (void*)0;
}

void* Visitor::Visit(FunctionChain* node)
{
  return (void*)0;
}

void* Visitor::Visit(Block* node)
{
  return (void*)0;
}

void* Visitor::Visit(Call* node)
{
  return (void*)0;
}

void* Visitor::Visit(Variable* node)
{
  return (void*)0;
}

void* Visitor::Visit(Function* node)
{
  return (void*)0;
}

void* Visitor::Visit(Condition* node)
{
  return (void*)0;
}

void* Visitor::Visit(Conditions* node)
{
  return (void*)0;
}

void* Visitor::Visit(Conditional* node)
{
  return (void*)0;
}

void* ControlBlockDetectionVisitor::Visit(Program* node)
{
  return (void*)false;
}

void* ControlBlockDetectionVisitor::Visit(Arguments* node)
{
  return (void*)false;
}

void* ControlBlockDetectionVisitor::Visit(FunctionChain* node)
{
  return (void*)false;
}

void* ControlBlockDetectionVisitor::Visit(Block* node)
{
  return (void*)true;
}

void* ControlBlockDetectionVisitor::Visit(Call* node)
{
  return (void*)false;
}

void* ControlBlockDetectionVisitor::Visit(Variable* node)
{
  return (void*)false;
}

void* ControlBlockDetectionVisitor::Visit(Function* node)
{
  return (void*)true;
}

void* ControlBlockDetectionVisitor::Visit(Condition* node)
{
  return (void*)false;
}

void* ControlBlockDetectionVisitor::Visit(Conditions* node)
{
  return (void*)false;
}

void* ControlBlockDetectionVisitor::Visit(Conditional* node)
{
  return (void*)true;
}
