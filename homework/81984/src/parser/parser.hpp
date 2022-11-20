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
  Function* ParseFunction();
  Conditional* ParseConditional();
  Variable* ParseVariable();
  Call* ParseCall();

  Arguments ParseArguments(bool required_parenthesis = false);
  Conditions ParseConditions();
  FunctionChain ParseFunctionChain();
  Block ParseBlock();

  Token Current();
  Token CurrentMove();
  void Assert(TokenType type);
  void AssertMove(TokenType type);
};

#endif
