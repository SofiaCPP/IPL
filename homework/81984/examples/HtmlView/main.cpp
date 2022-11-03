#include "../../src/HtmlSyntaxHighlighter.hpp"

using namespace Rb;

int main(int argc, const char* argv[]) {
  if (argc <= 1) return 1;

  HtmlSyntaxHighlighter({ .ShowTokenInformationOnHover = true }).Highlight(argv[1]);

  return 0;
}
