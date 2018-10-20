#ifndef FMI_IPL_LEXCER_H
#define FMI_IPL_LEXCER_H

#include <unordered_set>
#include <unordered_map>
#include <string>

#include <cstdio>

namespace LexCer {
  
enum class TokenType : short {
  Operator,
  Keyword,
  PreprocessorDirective,
  Typename,
  Identifier,
  String,
  Comment,
  Number,
  Skip,
  Eof,
  Invalid
};

struct Token {
  TokenType type;
  unsigned line;
  std::string lexeme;
  double number;
};

std::string getOutputFileName(const std::string &name);
void lexCit(FILE *in, FILE *out);

} /* Namespace LexCer */

#endif
