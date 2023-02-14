#include "../src/lexer.hpp"
#include "../src/parser.hpp"
#include "../src/expression/expression_serialization_visitor.hpp"

int main()
{
  string program = read_file("main.rb");

  lexer lex(program);
  parser par(lex.tokenize());
  expression_serialization_visitor vis;

  expression_ptr expression = par.parse_program_expression();
  expression->accept(vis);
  LOG("HERE");
  vis.to_json("main.json");

  return 0;
}
