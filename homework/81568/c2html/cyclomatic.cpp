#include "cyclomatic.h"
#include "html_utils.h"
#include "lexcer.h"
#include "lexconst.h"
#include <stack>

using HTMLUtils::writeToOutput;
using LexCer::TokenClass;
using LexCer::Tokenizer;

void setNextClassName(std::string *name, unsigned counter) {
  while (name->back() != 'a')
    name->pop_back();
  name->append(std::to_string(counter));
}

// Writing a full parser would give us the change of
// making the cyclomatic graph of the function.
// #courseproject
unsigned ParCer::outputWithCyclomaticComplexity(FILE *in, FILE *out) {
  Tokenizer tokenizer (in, out);

  std::string lexeme;
  double number;
  TokenClass type;
  Token currentToken;
  std::stack<short> history;
  bool inFunction = false;

  #define PAREN false
  std::stack<bool> openedBlocks;
  std::string className = "a";
  unsigned counter = 1;
  unsigned complexity = 1;
  
  do {
    currentToken = tokenizer.nextToken(&lexeme, &number);
    type = currentToken.type;
    
    if (!inFunction) {
      history.push(type);
    } else {
      if (type == TokenClass::RightCurly) {
        if (openedBlocks.empty()) { // end of function. What one does to avoid parsing 'C'
          inFunction = false;
          fputs("<!-- ", out);
          char closingDiv[100];
          sprintf(closingDiv, "%d --></div>", complexity);
          fputs(closingDiv, out);
          complexity = 1;
        } else {
          openedBlocks.pop();
        }
      }
      else if (type == TokenClass::LeftCurly) {
        openedBlocks.push(PAREN);
      }
      else if (type == TokenClass::Keyword) {
        if (!lexeme.compare("if") ||
            !lexeme.compare("while") ||
            !lexeme.compare("for") ||
            !lexeme.compare("case") ||
            !lexeme.compare("default"))
          {
          ++complexity;
        }
      }
    }

    if (!inFunction && type == TokenClass::LeftCurly) {
      history.pop();
      while (!history.empty() && history.top() == TokenClass::Skip) {
        history.pop();
      }
      
      if (history.top() == TokenClass::RightParen) {
        while (!history.empty() && history.top() != TokenClass::LeftParen)
          history.pop();
        if (!history.empty())
          history.pop();
        while (!history.empty() && history.top() == TokenClass::Skip)
          history.pop();

        if (!history.empty() && history.top() == TokenClass::Identifier) { // We are in a function
          char openingDiv[100];
          setNextClassName(&className, counter);
          ++counter;
          sprintf(openingDiv, "<div class=\"%s\">", className.c_str()); // this has the side effect of displaying the 
          fputs(openingDiv, out);
          inFunction = true;
          history = std::stack<short>{};
        }
      }
    }
    
    if (!writeToOutput(out, currentToken))
      break;
    lexeme = "";
    number = 0.0;
  } while (type != TokenClass::Eof ||
           type != TokenClass::Invalid);
  return counter - 1;
}
