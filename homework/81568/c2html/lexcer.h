#ifndef FMI_IPL_LEXCER_H
#define FMI_IPL_LEXCER_H

#include <unordered_set>
#include <unordered_map>
#include <string>

#include <cstdio>

namespace LexCer {
  
enum TokenClass : short {
  Operator,
  Keyword,
  PreprocessorDirective,
  Typename,
  Identifier,
  String,
  Comment,
  Number,
  LeftParen,
  RightParen,
  LeftCurly,
  RightCurly,
  
  Skip,
  Eof,
  Invalid
};

struct Token {
  TokenClass type;
  unsigned line;
  std::string lexeme;
  double number;
};


class Tokenizer {
private:
  enum num_t {
              BIN = 2,
              OCT = 8,
              DEC = 10,
              HEX = 16
  };

public:
  Tokenizer(FILE *in, FILE *out);

  Token nextToken(std::string *lexeme, double *number);
  
private:
  Token makeToken(TokenClass type, std::string lexeme = std::string(), double number = 0.0);
  bool readNextDirective(std::string *lexeme);
  bool readNextString(std::string *lexeme, char oldc);
  bool readNextIdentifier(std::string *lexeme);
  bool readNextNumber(char beg, double *number, std::string *numberRepr);
  bool isValidDigit(char c, num_t numType) const ;
  bool readLineComment(std::string *lexeme);
  bool readMultilineComment(std::string *lexeme);
  bool readInclude(std::string *lexeme);
  bool readUntilBlank(std::string *lexeme);

  double atod(const std::string &number) const { return 0.0; }
private:              
  FILE *in;
  FILE *out;
  Token token; // current token
  unsigned line = 1;
  std::unordered_set<std::string> typenames;
};
  
} /* Namespace LexCer */

#endif
