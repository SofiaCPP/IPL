#include "../../src/html/control_dropdown.hpp"

int main(int argc, const char* argv[]) {
  if (argc <= 1) return 1;

  ControlDropdown().ProcessFile(argv[1]);

  return 0;
}
