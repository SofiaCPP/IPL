#include "lexcer.h"
#include "lexconst.h"

#include <cctype>
#include <cstdlib>

namespace LexCer {

Tokenizer::Tokenizer(FILE *in, FILE *out)
  : in(in), out(out) {
  token.type = TokenClass::Invalid;
  // The idea behind having modifiable typenames structure was to read #defines, but 
  // maybe it is better if a whole preproccessor is written. 
  typenames = {"void", "char", "short", "int", "long", "float", "double", "signed", "unsigned", "FILE"};
}

Token Tokenizer::makeToken(TokenClass type, std::string lexeme, double number) {
  token.type = type;
  token.line = line;
  token.lexeme = lexeme;
  token.number = number;
  return token;
}

bool Tokenizer::readNextDirective(std::string *lexeme) {
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

bool Tokenizer::readNextString(std::string *lexeme, char oldc) {
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

bool Tokenizer::readNextIdentifier(std::string *lexeme) {
  char c;
  bool valid = true;
  while (isalnum(c = fgetc(in)) || c == '_') {
    lexeme->push_back(c);
  }
  ungetc((unsigned) c, in);
  return valid;
}

bool Tokenizer::readNextNumber(char beg, double *number, std::string *numberRepr) {
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

bool Tokenizer::isValidDigit(char c, num_t numType) const {
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

bool Tokenizer::readLineComment(std::string *lexeme) {
  char c;
  while ((c = fgetc(in)) != '\n' && c != EOF)
    lexeme->push_back(c);
  ungetc((unsigned) c, in);
  return true; // for consistency
}

bool Tokenizer::readMultilineComment(std::string *lexeme) {
  char c, prevc = (char) -1;
  while (((c = fgetc(in)) != '/' && prevc != '*') && c != EOF)
    lexeme->push_back(c);
  
  if (c == EOF) {
    return false;
  }
  lexeme->push_back(c); // closing slash
  return true;
}

bool Tokenizer::readInclude(std::string *lexeme) {
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

bool Tokenizer::readUntilBlank(std::string *lexeme) {
  char c;
  while (!isblank(c = fgetc(in)) && c != EOF) ;
  ungetc((unsigned) c, in);
  return true;
}

Token Tokenizer::nextToken(std::string *lexeme, double *number) {
  TokenClass type;
  if (token.type == TokenClass::Eof) {
    return makeToken(TokenClass::Invalid, *lexeme, *number);
  }
  
  char c = fgetc(in);
  if (c == EOF) {
    return makeToken(TokenClass::Eof, *lexeme, *number);
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
    return makeToken(TokenClass::Skip, *lexeme, *number);
  case '(':
    return makeToken(TokenClass::LeftParen, *lexeme, *number);
  case ')':
    return makeToken(TokenClass::RightParen, *lexeme, *number);
  case '{':
    return makeToken(TokenClass::LeftCurly, *lexeme, *number);
  case '}':
    return makeToken(TokenClass::RightCurly, *lexeme, *number);
  case '[':
  case ']':
  case '?':
  case ':':
    return makeToken(TokenClass::Operator, *lexeme, *number);
  case '*':
    c = fgetc(in);
    if (c == '/') {
      return makeToken(TokenClass::Invalid, *lexeme, *number);
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
    return makeToken(TokenClass::Operator, *lexeme, *number);
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
        return makeToken(TokenClass::Invalid, *lexeme, *number);
      return makeToken(TokenClass::String, *lexeme, *number);
    } else
      ungetc((unsigned) c, in);
    return makeToken(TokenClass::Operator, *lexeme, *number);
  case '&':
  case '|':
  case '+':
    c = fgetc(in);
    if (c == lexeme->at(lexeme->size() - 1) || c == '=')
      lexeme->push_back(c);
    else
      ungetc((unsigned) c, in);
    return makeToken(TokenClass::Operator, *lexeme, *number);
  case '-':
    c = fgetc(in);
    if (c == '-' || c == '=' || c == '>')
      lexeme->push_back(c);
    else
      ungetc((unsigned) c, in);
    return makeToken(TokenClass::Operator, *lexeme, *number);
  case '#':
    if (readNextDirective(lexeme))
      return makeToken(TokenClass::PreprocessorDirective, *lexeme, *number);
    return makeToken(TokenClass::Invalid, *lexeme, *number);
  case '\'':
    lexeme->push_back(c = fgetc(in));
    if (c == '\\') {
      lexeme->push_back(c = fgetc(in));
    }
    lexeme->push_back(c = fgetc(in)); // the closing mark
    if (c != '\'')
      return makeToken(TokenClass::Invalid, *lexeme, *number);
    return makeToken(TokenClass::String, *lexeme, *number);
  case '"':
    if (readNextString(lexeme, c))
      return makeToken(TokenClass::String, *lexeme, *number);
    return makeToken(TokenClass::Invalid, *lexeme, *number);
  case '/':
    c = fgetc(in);
    lexeme->push_back(c);
    switch (c) {
    case '=':
      return makeToken(TokenClass::Operator, *lexeme, *number);
    case '/':
      if (!readLineComment(lexeme))
        return makeToken(TokenClass::Invalid, *lexeme, *number);
      return makeToken(TokenClass::Comment, *lexeme, *number);
    case '*':
      if (!readMultilineComment(lexeme))
        return makeToken(TokenClass::Invalid, *lexeme, *number);
      return makeToken(TokenClass::Comment, *lexeme, *number);
    default:
      if (!isblank(c) || !isdigit(c)) {
        ungetc((unsigned) c, in);
        lexeme->pop_back();
        return makeToken(TokenClass::Invalid, *lexeme, *number);
      }
      return makeToken(TokenClass::Operator, *lexeme, *number);
    }
  case '\\':
    c = fgetc(in);
    ungetc((unsigned) c, in);
    if (c != '\n')
      return makeToken(TokenClass::Invalid, *lexeme, *number); // stray '\'
    return makeToken(TokenClass::Operator, *lexeme, *number);
  }
  
  // identifier || keyword || typename
  if (isalpha(c) || c == '_') {
    if (readNextIdentifier(lexeme)) {
      if (keywords.count(*lexeme) != 0)
        return makeToken(TokenClass::Keyword, *lexeme, *number);
      else if (typenames.count(*lexeme) != 0)
        return makeToken(TokenClass::Typename, *lexeme, *number);
      return makeToken(TokenClass::Identifier, *lexeme, *number);
    } else {
      return makeToken(TokenClass::Invalid, *lexeme, *number);
    }
  }
  
  if (!isdigit(c)) {
    return makeToken(TokenClass::Invalid, *lexeme, *number);
  }
  
  // number
  if (readNextNumber(c, number, lexeme))
    return makeToken(TokenClass::Number, *lexeme, *number);
  return makeToken(TokenClass::Invalid, *lexeme, *number);
}
  
} /* Namespace Lexcer */
