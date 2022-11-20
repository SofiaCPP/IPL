#include "parser.hpp"

Parser::Parser(Vector<Token> tokens) : tokens_(tokens), current_(0)
{
}

Program Parser::ParseProgram()
{
  Program program;

  while (Current().type != EndOfFile/*ProgramTypes*/)
  {
    Node* node;

    switch (Current().type)
    {
      case Def: node = ParseFunction();    break;
      case If:  node = ParseConditional(); break;
      case Identifier:
      {
        if (tokens_[current_ + 1].type == Equal)
        {
          node = ParseVariable();
        }
        else
        {
          node = ParseCall();
        }
        break;
      }
      default: throw;
    }

    program.nodes.push_back(node);
  }

  return program;
}

Function* Parser::ParseFunction()
{
  Function* function = new Function();

  AssertMove(Def);

  Assert(Identifier);
  function->identifier = CurrentMove().data;

  if (Current().type == OpenParenthesis)
  {
    function->arguments = ParseArguments();
    AssertMove(ClosedParenthesis);
  }

  AssertMove(NewLine);

  function->body = ParseProgram();

  AssertMove(End);
  AssertMove(NewLine);

  return function;
}

Conditional* Parser::ParseConditional()
{
  Conditional* conditional = new Conditional();

  AssertMove(If);

  conditional->conditions = ParseConditions();

  AssertMove(NewLine);

  conditional->body = ParseProgram();

  while (Current().type == Elsif)
  {
    AssertMove(Elsif);

    Conditional acondition;

    acondition.conditions = ParseConditions();
    acondition.body = ParseProgram();

    conditional->aconditions.push_back(acondition);
  }

  if (Current().type == Else)
  {
    AssertMove(Else);

    Conditional acondition;

    acondition.body = ParseProgram();

    conditional->aconditions.push_back(acondition);
  }

  AssertMove(End);
  AssertMove(NewLine);

  return conditional;
}

Variable* Parser::ParseVariable()
{
  Variable* variable = new Variable();

  Assert(Identifier);

  variable->identifier = CurrentMove().data;

  AssertMove(Equal);

  variable->value = CurrentMove().data;

  AssertMove(NewLine);

  return variable;
}

Call* Parser::ParseCall()
{
  Call* call = new Call();

  call->caller = CurrentMove().data;

  if (Current().type == Dot) call->fchain = ParseFunctionChain();
  if (Current().type == Do) call->block = ParseBlock();

  AssertMove(NewLine);

  return call;
}

FunctionChain Parser::ParseFunctionChain()
{
  FunctionChain fchain;

  while (CurrentMove().type == Dot)
  {
    Assert(Identifier);
    fchain.identifier.push_back(CurrentMove().data);

    if (Current().type == OpenParenthesis)
    {
      fchain.arguments.push_back(ParseArguments(true));
    }
    else
    {
      fchain.arguments.push_back({});
    }
  }

  return fchain;
}

Block Parser::ParseBlock()
{
  Block block;

  AssertMove(Do);

  if (Current().type == StraightLine)
  {
    AssertMove(StraightLine);

    block.arguments = ParseArguments();

    AssertMove(StraightLine);
  }

  block.body = ParseProgram();

  AssertMove(End);

  return block;
}

Arguments Parser::ParseArguments(bool required_parenthesis)
{
  Arguments arguments;

  if (required_parenthesis) AssertMove(OpenParenthesis);

  while (Current().type == Identifier)
  {
    arguments.variables.push_back(CurrentMove().data);

    if (Current().type == Comma) AssertMove(Comma);
  }

  if (required_parenthesis) AssertMove(ClosedParenthesis);

  return arguments;
}

Conditions Parser::ParseConditions()
{
  Conditions conditions;
  return conditions;
}

Token Parser::Current()
{
  return tokens_[current_];
}

Token Parser::CurrentMove()
{
  return tokens_[current_++];
}

void Parser::Assert(TokenType type)
{
  if (Current().type != type) throw;
}

void Parser::AssertMove(TokenType type)
{
  if (CurrentMove().type != type) throw;
}
