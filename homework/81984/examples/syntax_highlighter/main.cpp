#include "../../src/html/syntax_highlighter.hpp"

int main(int argc, const char* argv[]) {
  if (argc <= 1) return 1;

  SyntaxHighlighter({ .hover_information = true }).ProcessFile(argv[1]);

  return 0;
}
