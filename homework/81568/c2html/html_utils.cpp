#include "html_utils.h"
#include "lexconst.h"

namespace HTMLUtils {

std::string escapeHTML(const std::string &lexeme) {
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
  
bool writeToOutput(FILE *out, Token token) {
  if (token.type == TokenClass::Eof) {
    printf("End of file!\n");
    fputc('\n', out);
    return false;
  }

  if (token.type == TokenClass::Skip) {
    if (fputs(token.lexeme.c_str(), out) == EOF) {
      printf("Error while writing to output");
      return false;
    }
    return true;
  }
  
  printf("Token of type %s at line %d: ", LexCer::tokenToClass.at(token.type).c_str(), token.line);
  if (token.type == TokenClass::Number)
    printf("%f\n", token.number);
  else
    printf("%s\n", token.lexeme.c_str());

  auto lexeme = escapeHTML(token.lexeme);
  
  std::string output;
  output.append(LexCer::spanBegin).append(LexCer::tokenToClass.at(token.type)).append("\">").append(lexeme).append(LexCer::spanEnd);
  
  if (fputs(output.c_str(), out) == EOF) {
    printf("Error while writing to output");
    return false;
  }
  return true;
}
  
std::string getOutputFileName(const std::string &name) {
  auto pos = name.find_last_of('.');
  if (pos == std::string::npos) {
    return name + ".html";
  }
  
  return name.substr(0, pos) + ".html";
}

void lexCit(FILE *in, FILE *out) {
  Tokenizer hl {in, out};
 
  std::string lexeme;
  double number;
  TokenClass type;
  Token token;
  do {
    token = hl.nextToken(&lexeme, &number);
    type = token.type;
    
    if (!writeToOutput(out, token))
      break;
    lexeme = "";
    number = 0.0;
  } while(type != TokenClass::Eof || type != TokenClass::Invalid);
}

} /* Namespace HTMLUtils */
