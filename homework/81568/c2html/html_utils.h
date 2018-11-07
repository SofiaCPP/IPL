#ifndef HTML_UTILS_H
#define HTML_UTILS_H

#include "lexcer.h"
using LexCer::Token;
using LexCer::TokenClass;
using LexCer::Tokenizer;

namespace HTMLUtils {
  
std::string escapeHTML(const std::string &lexeme);
bool writeToOutput(FILE *out, Token token);
std::string getOutputFileName(const std::string &name);
void lexCit(FILE *in, FILE *out);
  
} /* Namespace HMTLUtils */

#endif /* HTML_UTILS_H */
