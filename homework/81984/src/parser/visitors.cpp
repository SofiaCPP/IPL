#include "visitors.hpp"

#define TAB_SIZE 2

DisplayVisitor::DisplayVisitor() : tabulation(0)
{
}

String DisplayVisitor::Visit(Program* node)
{
  String display = "";

  for (SharedPtr<Node> program_node : node->nodes)
  {
    display += program_node->Accept(*this);
    display += highlighter.ProcessToken({ .type = NewLine });
  }

  return display;
}

String DisplayVisitor::Visit(Arguments* node)
{
  String display = "";

  for (Index it = 0; it < node->variables.size(); it++)
  {
    display += highlighter.ProcessToken({ .type = Identifier, .data = node->variables[it] });

    if (it < node->variables.size() - 1)
    {
      display += highlighter.ProcessToken({ .type = Comma, .data = "," });
      display += highlighter.ProcessToken({ .type = Space, .data = " " });
    }
  }

  return display;
}

String DisplayVisitor::Visit(FunctionChain* node)
{
  String display = "";

  for (Index it = 0; it < node->identifiers.size(); ++it)
  {
    if (it < node->identifiers.size() - 1)
    {
      display += highlighter.ProcessToken({ .type = Dot, .data = "." });
    }

    display += highlighter.ProcessToken({ .type = Identifier, .data = node->identifiers[it] });

    if (node->arguments[it].variables.size() > 0)
    {
      display += highlighter.ProcessToken({ .type = OpenParenthesis, .data = "(" });
      display += node->arguments[it].Accept(*this);
      display += highlighter.ProcessToken({ .type = ClosedParenthesis, .data = ")" });
    }
  }

  return display;
}

String DisplayVisitor::Visit(Block* node)
{
  String display = "";

  display += highlighter.ProcessToken({ .type = Space, .data = " " });
  display += highlighter.ProcessToken({ .type = Do, .data = "do" });

  if (node->arguments.variables.size() > 0)
  {
    display += highlighter.ProcessToken({ .type = Space, .data = " " });
    display += highlighter.ProcessToken({ .type = StraightLine, .data = "|" });
    display += node->arguments.Accept(*this);
    display += highlighter.ProcessToken({ .type = StraightLine, .data = "|" });
  }

  display += highlighter.ProcessToken({ .type = NewLine });

  tabulation += TAB_SIZE;
  display += node->body.Accept(*this);
  tabulation -= TAB_SIZE;

  display += Tab();
  display += highlighter.ProcessToken({ .type = End, .data = "end" });

  return display;
}

String DisplayVisitor::Visit(Call* node)
{
  String display = Tab();

  display += highlighter.ProcessToken({ .type = Identifier, .data = node->caller });
  display += node->fchain.Accept(*this);
  display += node->block.Accept(*this);

  return display;
}

String DisplayVisitor::Visit(Variable* node)
{
  String display = Tab();

  display += highlighter.ProcessToken({ .type = Identifier, .data = node->identifier });
  display += highlighter.ProcessToken({ .type = Space, .data = " " });
  display += highlighter.ProcessToken({ .type = Equal, .data = "=" });
  display += highlighter.ProcessToken({ .type = Space, .data = " " });
  display += highlighter.ProcessToken({  .type = Identifier, .data = node->value });

  return display;
}

String DisplayVisitor::Visit(Function* node)
{
  String display = Tab();

  display += highlighter.ProcessToken({ .type = Def, .data = "def" });
  display += highlighter.ProcessToken({ .type = Space, .data = " " });
  display += highlighter.ProcessToken({ .type = Identifier, .data = node->identifier });

  if (node->arguments.variables.size() > 0)
  {
    display += highlighter.ProcessToken({ .type = OpenParenthesis, .data = "(" });
    display += node->arguments.Accept(*this);
    display += highlighter.ProcessToken({ .type = ClosedParenthesis, .data = ")" });
  }

  display += highlighter.ProcessToken({ .type = NewLine });

  tabulation += TAB_SIZE;
  display += node->body.Accept(*this);
  tabulation -= TAB_SIZE;

  display += Tab();
  display += highlighter.ProcessToken({ .type = End, .data = "end" });

  return display;
}

String DisplayVisitor::Visit(Condition* node)
{
  String display = "";

  display += highlighter.ProcessToken({ .type = Identifier, .data = node->lhs });
  display += highlighter.ProcessToken({ .type = Space, .data = " " });
  display += highlighter.ProcessToken({ .type = Identifier, .data = node->op });
  display += highlighter.ProcessToken({ .type = Space, .data = " " });
  display += highlighter.ProcessToken({ .type = Identifier, .data = node->rhs });

  return display;
}

String DisplayVisitor::Visit(Conditions* node)
{
  String display = "";

  for (Index it = 0; it < node->conditions.size(); it++)
  {
    display += node->conditions[it].Accept(*this);

    if (it < node->lchain.size())
    {
      display += highlighter.ProcessToken({ .type = Space, .data = " " });
      display += highlighter.ProcessToken({ .type = Identifier, .data = node->lchain[it] });
      display += highlighter.ProcessToken({ .type = Space, .data = " " });
    }
  }

  return display;
}

String DisplayVisitor::Visit(Conditional* node)
{
  String display = Tab();

  display += highlighter.ProcessToken({ .type = If, .data = "if" });
  display += highlighter.ProcessToken({ .type = Space, .data = " " });
  display += node->conditions.Accept(*this);
  display += highlighter.ProcessToken({ .type = NewLine });

  tabulation += TAB_SIZE;
  display += node->body.Accept(*this);
  tabulation -= TAB_SIZE;

  display += Tab();
  display += highlighter.ProcessToken({ .type = End, .data = "end" });

  for (Index it = 0; it < node->aconditions.size(); it++)
  {
    display += highlighter.ProcessToken({ .type = NewLine });
    display += node->aconditions[it].Accept(*this);
  }

  return display;
}

String DisplayVisitor::Tab()
{
  String display = "";

  for (Index it = 0; it < tabulation; it++)
  {
    display += highlighter.ProcessToken({ .type = Space, .data = " " });
  }

  return display;
}
