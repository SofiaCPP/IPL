#include "../src/lexer.hpp"
#include "../src/parser.hpp"
#include "../src/expression/expression_serialization_visitor.hpp"

int main()
{
  string program = read_file("main.rb");

  lexer lex(program);
  parser par(lex.tokenize());
  expression_serialization_visitor vis;

  ptr<expression> expression = par.parse_program_expression();
  expression->accept(vis);
  vis.output("main.json");

  return 0;
}
