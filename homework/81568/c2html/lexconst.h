#ifndef LEXCER_CONST_H
#define LEXCER_CONST_H

namespace LexCer {

static const std::unordered_set<std::string> keywords = 
  {"auto", "register", "static", "extern", "typedef", "struct", "union", "enum",
   "if", "else", "for", "while", "switch", "case", "default", "const", "break",
   "continue", "goto", "sizeof", "alignof", "volatile", "return"};

static const std::unordered_set<std::string> preprocessor =
  {"include", "define", "undef", "if", "ifdef", "ifndef", "else", "elif", "endif",
   "line", "error", "pragma"};

static const std::unordered_map<TokenType, std::string> tokenToClass
  {
   {
    {TokenType::Operator, "operator"},
    {TokenType::Keyword, "keyword"},
    {TokenType::Typename, "typename"},
    {TokenType::PreprocessorDirective, "preproc"},
    {TokenType::Identifier, "identifier"},
    {TokenType::String, "string"},
    {TokenType::Comment, "comment"},
    {TokenType::Number, "number"},
    {TokenType::Invalid, "invalid"}
   }
  };

static const std::string spanBegin = "<span class=\"";
static const std::string spanEnd = "</span>";

static constexpr int MAX_EXP_LEN = 4;

} /* Namespace LexCer */
  
#endif
