#ifndef RUBY_IPL_PARSER_HPP_
#define RUBY_IPL_PARSER_HPP_

#include "../utils/types.hpp"
#include "../lexer/token.hpp"
#include "nodes.hpp"

class Parser
{
  Vector<Token> tokens_;
  Index current_;
public:
  Parser(Vector<Token> tokens);

  Program ParseProgram();
private:
  SharedPtr<Function> ParseFunction();
  SharedPtr<Conditional> ParseConditional();
  SharedPtr<Variable> ParseVariable();
  SharedPtr<Call> ParseCall();

  Arguments ParseArguments();
  FunctionChain ParseFunctionChain();
  Block ParseBlock();
  Conditions ParseConditions();
  Condition ParseCondition();

  Token Current();
  Token CurrentMove();
  bool CurrentLogical();
  bool CurrentCallable();
  void Assert(TokenType type);
  void AssertMove(TokenType type);
};

#endif
