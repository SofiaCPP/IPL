#include "parser.tab.h"

int main(int argc, char** argv){
  void* scanner;
  yylex(&scanner);
  yyset_in(stdin, scanner);

  int rvalue = yyparse(scanner);

}
