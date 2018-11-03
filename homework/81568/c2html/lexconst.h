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

static const std::unordered_map<TokenClass, std::string> tokenToClass
  {
   {
    {TokenClass::Operator, "operator"},
    {TokenClass::Keyword, "keyword"},
    {TokenClass::Typename, "typename"},
    {TokenClass::PreprocessorDirective, "preproc"},
    {TokenClass::Identifier, "identifier"},
    {TokenClass::String, "string"},
    {TokenClass::Comment, "comment"},
    {TokenClass::Number, "number"},
    {TokenClass::Invalid, "invalid"},
    {TokenClass::LeftParen, "identifier"},
    {TokenClass::RightParen, "identifier"}
   }
  };

static const std::string spanBegin = "<span class=\"";
static const std::string spanEnd = "</span>";

static constexpr int MAX_EXP_LEN = 4;

} /* Namespace LexCer */
  
#endif
