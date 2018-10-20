#include "lexcer.h"
#include "lexconst.h"

#include <cctype>
#include <cstdlib>

using LexCer::TokenType;
using LexCer::Token;
using LexCer::keywords;
using LexCer::preprocessor;
using LexCer::tokenToClass;
using LexCer::spanBegin;
using LexCer::spanEnd;

class Highlighter {
private:
  enum num_t {
              BIN = 2,
              OCT = 8,
              DEC = 10,
              HEX = 16
  };

public:
  Highlighter(FILE *in, FILE *out);

  void outputHighlighted();

private:
  std::string escapeHTML(const std::string &lexeme);
  TokenType nextToken(std::string *lexeme, double *number);
  void makeToken(TokenType type, unsigned line,
                 std::string lexeme = std::string(), double number = 0.0);
  bool readNextDirective(std::string *lexeme);
  bool readNextString(std::string *lexeme, char oldc);
  bool readNextIdentifier(std::string *lexeme);
  bool readNextNumber(char beg, double *number, std::string *numberRepr);
  bool isValidDigit(char c, num_t numType) const ;
  bool readLineComment(std::string *lexeme);
  bool readMultilineComment(std::string *lexeme);
  bool readInclude(std::string *lexeme);
  bool readUntilBlank(std::string *lexeme);
  bool writeToOutput();

  double atod(const std::string &number) const { return 0.0; }
private:              
  FILE *in;
  FILE *out;
  Token token; // current token
  unsigned line = 1;
  std::unordered_set<std::string> typenames;
};

//const char* Highlighter::spanBegin = "<span class=\"";
//const char* Highlighter::spanEnd = "</span>";

Highlighter::Highlighter(FILE *in, FILE *out)
  : in(in), out(out) {
  token.type = TokenType::Invalid;
  typenames = {"void", "char", "short", "int", "long", "float", "double", "signed", "unsigned"};
}

void Highlighter::outputHighlighted() {
  std::string lexeme;
  double number;
  TokenType type;
  do {
    type = nextToken(&lexeme, &number);
    
    makeToken(type, line, lexeme, number);
    if (!writeToOutput())
      break;
    lexeme = "";
    number = 0.0;
  } while(token.type != TokenType::Eof || token.type != TokenType::Invalid);
}

void Highlighter::makeToken(TokenType type, unsigned line, std::string lexeme, double number) {
  token.type = type;
  token.line = line;
  token.lexeme = lexeme;
  token.number = number;
}

bool Highlighter::readNextDirective(std::string *lexeme) {
  char c;
  while (isalpha((c = fgetc(in))))
    lexeme->push_back(c);
  // we may need that last bit
  ungetc((unsigned) c, in);
  auto directive = lexeme->substr(1, lexeme->size() - 1);
  if (!preprocessor.count(directive))
    return false;
  if (directive.compare("define") == 0) {
    //readDefine();
  }
  return true;
}

bool Highlighter::readNextString(std::string *lexeme, char oldc) {
  char c, prevc = oldc;
  while (((c = fgetc(in)) != '"' || prevc == '\\') && c != '\n' && c != EOF) {
    lexeme->push_back(c);
    prevc = c;
  }
  
  if (c == '\n' || c == EOF) {
    ungetc((unsigned) c, in);
    return false;
  }
  lexeme->push_back(c);
  return true;
}

bool Highlighter::readNextIdentifier(std::string *lexeme) {
  char c;
  bool valid = true;
  while (isalnum(c = fgetc(in)) || c == '_') {
    lexeme->push_back(c);
  }
  ungetc((unsigned) c, in);
  return valid;
}

bool Highlighter::readNextNumber(char beg, double *number, std::string *numberRepr) {
  bool valid = true;
  num_t base = DEC;
  char c;
  if (beg == '0') {
    c = fgetc(in);
    numberRepr->push_back(c);
    if (c == 'b')
      base = BIN;
    else if (c == 'x' || c == 'X')
      base = HEX;
    else if (isdigit(c))
      base = OCT;
    else
      ungetc((unsigned) c, in);
  }

  while (isValidDigit(c = fgetc(in), base)) {
    numberRepr->push_back(c);
  }

  if (c == '.') {
    numberRepr->push_back(c);
    while (isValidDigit(c = fgetc(in), base))
      numberRepr->push_back(c);
  }
  if (tolower(c) == 'e') {
    numberRepr->push_back(c);
    if (base != DEC)
      return false;
    if (!isdigit(c = fgetc(in)) && c != '-' && c != '+') {
      ungetc((unsigned) c, in);
      return false;
    }
    numberRepr->push_back(c);    
    while (isValidDigit(c = fgetc(in), base))
      numberRepr->push_back(c);
  }
  
  char lower = tolower(c);
  if (lower == 'u') {
    numberRepr->push_back(c);
    lower = tolower(c = fgetc(in));
  }
  if (lower == 'f' || lower == 'l')
    numberRepr->push_back(c);
  else
    ungetc((unsigned) c, in);
  
  *number = this->atod(*numberRepr); // not working ...
  return true;
}

bool Highlighter::isValidDigit(char c, num_t numType) const {
  bool valid;
  switch (numType) {
  case DEC:
    return isdigit(c);
  case BIN:
    return (c == '0' || c == '1');
  case OCT:
    return (c >= '0' || c <= '7');
  case HEX:
    return (isdigit(c) || tolower(c) >= 'a' || tolower(c) <= 'f');
  default:
    return false;
  }
}

bool Highlighter::readLineComment(std::string *lexeme) {
  char c;
  while ((c = fgetc(in)) != '\n' && c != EOF)
    lexeme->push_back(c);
  ungetc((unsigned) c, in);
  return true; // for consistency
}

bool Highlighter::readMultilineComment(std::string *lexeme) {
  char c, prevc = (char) -1;
  while (((c = fgetc(in)) != '/' && prevc != '*') && c != EOF)
    lexeme->push_back(c);
  
  if (c == EOF) {
    return false;
  }
  lexeme->push_back(c); // closing slash
  return true;
}

bool Highlighter::readInclude(std::string *lexeme) {
  char c;
  bool valid = true;
  while ((c = fgetc(in)) != '>') {
    lexeme->push_back(c);
    if (!isalpha(c) && c != '_' && c != '.')
      valid = false;
  }
  lexeme->push_back(c);
  return valid;
}

bool Highlighter::readUntilBlank(std::string *lexeme) {
  char c;
  while (!isblank(c = fgetc(in)) && c != EOF) ;
  ungetc((unsigned) c, in);
  return true;
}

bool Highlighter::writeToOutput() {
  if (token.type == TokenType::Eof) {
    printf("End of file!\n");
    fputc('\n', out);
    return false;
  }

  if (token.type == TokenType::Skip) {
    if (fputs(token.lexeme.c_str(), out) == EOF) {
      printf("Error while writing to output");
      return false;
    }
    return true;
  }
  
  printf("Token of type %s at line %d: ", tokenToClass.at(token.type).c_str(), token.line);
  if (token.type == TokenType::Number)
    printf("%f\n", token.number);
  else
    printf("%s\n", token.lexeme.c_str());

  auto lexeme = escapeHTML(token.lexeme);
  
  std::string output;
  output.append(spanBegin).append(tokenToClass.at(token.type)).append("\">").append(lexeme).append(spanEnd);
  
  if (fputs(output.c_str(), out) == EOF) {
    printf("Error while writing to output");
    return false;
  }
  return true;
}

std::string Highlighter::escapeHTML(const std::string &lexeme) {
  std::string result;
  for (auto c : lexeme) {
    switch(c) {
    case '<':
      result.append("&lt");
      break;
    case '>':
      result.append("&gt");
      break;
    case '&':
      result.append("&amp");
      break;
    default:
      result.push_back(c);
      break;
    }
  }
  return result;
}

TokenType Highlighter::nextToken(std::string *lexeme, double *number) {
  if (token.type == TokenType::Eof) {
    return TokenType::Invalid;
  }
  
  char c = fgetc(in);
  if (c == EOF) {
    return TokenType::Eof;
  }
  lexeme->push_back(c);
  // operator || symbol || string || preprocessor directive
  // probably should check for errors like '5 * % 4;', but...
  switch(c) {
  case '\n':
    ++line;
  case '\t':
  case ' ':
  case ';':
  case ',':
  case '(':
  case ')':
  case '{':
  case '}':
    return TokenType::Skip;
  case '[':
  case ']':
  case '?':
  case ':':
    return TokenType::Operator;
  case '*':
    c = fgetc(in);
    if (c == '/') {
      return TokenType::Invalid;
    }
    ungetc((unsigned) c, in);
    c = '*';
  case '^':
  case '%':
  case '=':
  case '!':
  case '~':
    c = fgetc(in);
    if (c != '=') ungetc((unsigned) c, in);
    else lexeme->push_back(c);
    return TokenType::Operator;
  case '<':
  case '>':
    c = fgetc(in);
    if (c == lexeme->at(lexeme->size() - 1)) {
      lexeme->push_back(c);
      c = fgetc(in);
      if (c == '=')
        lexeme->push_back(c);
      else
        ungetc((unsigned) c, in);
    } else if (lexeme->at(lexeme->size() - 1) == '<' && isalpha(c)) {
      lexeme->push_back(c);
      if (!readInclude(lexeme))
        return TokenType::Invalid;
      return TokenType::String;
    } else
      ungetc((unsigned) c, in);
    return TokenType::Operator;
  case '&':
  case '|':
  case '+':
    c = fgetc(in);
    if (c == lexeme->at(lexeme->size() - 1) || c == '=')
      lexeme->push_back(c);
    else
      ungetc((unsigned) c, in);
    return TokenType::Operator;
  case '-':
    c = fgetc(in);
    if (c == '-' || c == '=' || c == '>')
      lexeme->push_back(c);
    else
      ungetc((unsigned) c, in);
    return TokenType::Operator;
  case '#':
    if (readNextDirective(lexeme))
      return TokenType::PreprocessorDirective;
    return TokenType::Invalid;
  case '\'':
    lexeme->push_back(c = fgetc(in));
    if (c == '\\') {
      lexeme->push_back(c = fgetc(in));
    }
    lexeme->push_back(c = fgetc(in)); // the closing mark
    if (c != '\'')
      return TokenType::Invalid;
    lexeme->push_back(c);
    return TokenType::String;
  case '"':
    if (readNextString(lexeme, c))
      return TokenType::String;
    return TokenType::Invalid;
  case '/':
    c = fgetc(in);
    lexeme->push_back(c);
    switch (c) {
    case '=':
      return TokenType::Operator;
    case '/':
      if (!readLineComment(lexeme))
        return TokenType::Invalid;
      return TokenType::Comment;
    case '*':
      if (!readMultilineComment(lexeme))
        return TokenType::Invalid;
      return TokenType::Comment;
    default:
      if (!isblank(c) || !isdigit(c)) {
        ungetc((unsigned) c, in);
        lexeme->pop_back();
        return TokenType::Invalid;
      }
      return TokenType::Operator;
    }
  case '\\':
    c = fgetc(in);
    ungetc((unsigned) c, in);
    if (c != '\n')
      return TokenType::Invalid; // stray '\'
    return TokenType::Operator;
  }
  
  // identifier || keyword || typename
  if (isalpha(c) || c == '_') {
    if (readNextIdentifier(lexeme)) {
      if (keywords.count(*lexeme) != 0)
        return TokenType::Keyword;
      else if (typenames.count(*lexeme) != 0)
        return TokenType::Typename;
      return TokenType::Identifier;
    } else {
      return TokenType::Invalid;
    }
  }
  
  if (!isdigit(c)) {
    return TokenType::Invalid;
  }
  
  // number
  if (readNextNumber(c, number, lexeme))
    return TokenType::Number;
  return TokenType::Invalid;
}


std::string LexCer::getOutputFileName(const std::string &name) {
  auto pos = name.find_last_of('.');
  if (pos == std::string::npos) {
    return name + ".html";
  }
  
  return name.substr(0, pos) + ".html";
}

void LexCer::lexCit(FILE *in, FILE *out) {
  Highlighter hl {in, out};
  printf("okay initiating\n");
  hl.outputHighlighted();
}

