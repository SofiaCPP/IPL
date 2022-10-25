#include "../../src/HtmlView.hpp"

using namespace Rb;

int main(int argc, const char* argv[]) {
  if (argc <= 1) return 1;

  HtmlView({ .ShowTokenInformationOnHover = true }).Visualize(argv[1]);

  return 0;
}
