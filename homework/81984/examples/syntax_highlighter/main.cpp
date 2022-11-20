#include "../../src/html/syntax_highlighter.hpp"

int main(int argc, const char* argv[]) {
  if (argc <= 1) return 1;

  SyntaxHighlighter({ .hover_information = true }).Highlight(argv[1]);

  return 0;
}
