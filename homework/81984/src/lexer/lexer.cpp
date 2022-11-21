#include "lexer.hpp"

#define KEYWORD_REGEX               Regex("^[a-z\\?]+")
#define IDENTIFIER_REGEX            Regex("^([a-zA-Z_]+)([a-zA-Z0-9_\\?]*)")
#define FUNCTION_IDENTIFIER_REGEX   Regex("^([a-zA-Z_]+)([a-zA-Z0-9_\\?]*)\\(")
#define SYMBOL_REGEX                Regex("^:(([a-zA-Z_]+)([a-zA-Z0-9_]*)|'.*'|\".*\")")
#define SINGLE_QUOTED_STRING_REGEX  Regex("^'.*'")
#define DOUBLE_QUOTED_STRING_REGEX  Regex("^\".*\"")
#define FUNCTION_IDENTIFIER_REGEX   Regex("^([a-zA-Z_]+)([a-zA-Z0-9_\\?]*)\\(")
#define NUMBER_REGEX                Regex("^[+-]?([0-9]*[.])?[0-9]+")
#define TRIPLE_DOT_REGEX            Regex("^\\.\\.\\.")
#define DOUBLE_DOT_REGEX            Regex("^\\.\\.")
#define TRIPLE_EQUAL_REGEX          Regex("^\\=\\=\\=")
#define DOUBLE_EQUAL_REGEX          Regex("^\\=\\=")
#define GREATER_THAN_OR_EQAUL_REGEX Regex("^>\\=")
#define LESS_THAN_OR_EQAUL_REGEX    Regex("^<\\=")
#define NO_REGEX_MATCH              String("")

Lexer::Lexer(LexerOptions options) : line_(0), column_(0), options_(options)
{
  keywords_["and"]    = And;
  keywords_["class"]  = Class;
  keywords_["def"]    = Def;
  keywords_["do"]     = Do;
  keywords_["else"]   = Else;
  keywords_["elsif"]  = Elsif;
  keywords_["end"]    = End;
  keywords_["if"]     = If;
  keywords_["module"] = Module;
  keywords_["or"]     = Or;
  keywords_["unless"] = Unless;
  keywords_["while"]  = While;
}

Vector<Token> Lexer::TokenizeExpression(CString& expression)
{
  line_ = 0;
  column_ = 0;

  Vector<Token> Tokens;

  while (*expression != '\0') Tokens.push_back(ReadNextToken(expression));

  Tokens.push_back({ .type = EndOfFile, .line = line_, .column = column_ });

  return Tokens;
}

Token Lexer::ReadNextToken(CString& expression)
{
  Token token = { .type = Unknown, .line = line_, .column = column_ };

  ReadWhitespace(expression, token);
  if (token.type != Unknown) return token;

  ReadSpecialCharacter(expression, token);
  if (token.type != Unknown) return token;

  ReadKeyword(expression, token);
  if (token.type != Unknown) return token;

  ReadIdentifier(expression, token);
  if (token.type != Unknown) return token;

  ReadSymbol(expression, token);
  if (token.type != Unknown) return token;

  ReadString(expression, token);
  if (token.type != Unknown) return token;

  ReadNumber(expression, token);
  if (token.type != Unknown) return token;

  if (options_.store_all_data) {
    token.data = *expression;
  }

  expression++;
  column_++;

  return token;
}

void Lexer::ReadWhitespace(CString& expression, Token& token)
{
  if (options_.tokenize_spaces)
  {
    if (*expression == ' ')
    {
      token.type = Space;

      if (options_.store_all_data) token.data = *expression;

      expression++;
      column_++;
    }
  }

  if (options_.tokenize_new_lines)
  {
    if (*expression == '\n')
    {
      token.type = NewLine;

      if (options_.store_all_data) token.data = *expression;

      expression++;
      column_ = 0;
      line_++;
    }
  }

  if (!options_.tokenize_spaces || !options_.tokenize_new_lines) ClearWhitespaces(expression);
}

void Lexer::ReadSpecialCharacter(CString& expression, Token& token)
{
  String matched_symbol = NO_REGEX_MATCH;

  switch (*expression)
  {
    case ',': token.type = Comma;               break;
    case '#': token.type = Octothorp;           break;
    case ')': token.type = ClosedParenthesis;   break;
    case '.': token.type = Dot;                 break;
    case '=': token.type = (matched_symbol = MatchRegex(expression, DOUBLE_EQUAL_REGEX)) != NO_REGEX_MATCH ? DoubleEqual : Equal; break;
    case '>': token.type = ((matched_symbol = MatchRegex(expression, GREATER_THAN_OR_EQAUL_REGEX)) != NO_REGEX_MATCH) ? GreaterThanOrEqual : GreaterThan; break;
    case '<': token.type = ((matched_symbol = MatchRegex(expression, LESS_THAN_OR_EQAUL_REGEX)) != NO_REGEX_MATCH) ? LessThanOrEqual : LessThan;          break;
    case '(': token.type = OpenParenthesis;     break;
    case '|': token.type = StraightLine;        break;
  }

  if (token.type != Unknown)
  {
    if (matched_symbol != NO_REGEX_MATCH)
    {
      if (options_.store_all_data) token.data = matched_symbol;

      expression += matched_symbol.length();
      column_ += matched_symbol.length();
    }
    else
    {
      if (options_.store_all_data) token.data = *expression;

      expression++;
      column_++;
    }
  }
}

void Lexer::ReadKeyword(CString& expression, Token& token)
{
  String matched_string = MatchRegex(expression, KEYWORD_REGEX);

  if (keywords_.find(matched_string) != keywords_.end())
  {
    token.type = keywords_[matched_string];

    if (options_.store_all_data) token.data = matched_string;

    expression += matched_string.length();
    column_ += matched_string.length();
  }
}

void Lexer::ReadIdentifier(CString& expression, Token& token)
{
  String matched_string = MatchRegex(expression, IDENTIFIER_REGEX);

  if (matched_string != NO_REGEX_MATCH)
  {
    token.type = Identifier;
    token.data = matched_string;

    expression += matched_string.length();
    column_ += matched_string.length();
  }
}

void Lexer::ReadSymbol(CString& expression, Token& token)
{
  String matched_string = MatchRegex(expression, SYMBOL_REGEX);

  if (matched_string != NO_REGEX_MATCH)
  {
    token.type = Symbol;
    token.data = matched_string;

    expression += matched_string.length();
    column_ += matched_string.length();
  }
}

void Lexer::ReadString(CString& expression, Token& token)
{
  String matched_string = MatchRegex(expression, SINGLE_QUOTED_STRING_REGEX);

  if (matched_string != NO_REGEX_MATCH)
  {
    token.type = SingleQuotedString;
    token.data = matched_string;

    expression += matched_string.length();
    column_ += matched_string.length();
  }
  else
  {
    matched_string = MatchRegex(expression, DOUBLE_QUOTED_STRING_REGEX);

    if (matched_string != NO_REGEX_MATCH)
    {
      token.type = DoubleQuotedString;
      token.data = matched_string;

      expression += matched_string.length();
      column_ += matched_string.length();
    }
  }
}

void Lexer::ReadNumber(CString& expression, Token& token) {
  String matched_string = MatchRegex(expression, NUMBER_REGEX);

  if (matched_string != NO_REGEX_MATCH)
  {
    token.type = Number;
    token.data = matched_string;

    expression += matched_string.length();
    column_ += matched_string.length();
  }
}

void Lexer::ClearWhitespaces(CString& expression) {
  while (*expression == ' ' || *expression == '\n')
  {
    if (*expression == '\n') {
      column_ = 0;
      line_++;
    } else {
      column_++;
    }

    expression++;
  }
}

String Lexer::MatchRegex(CString& expression, Regex regex)
{
  RegexCStringMatch match;
  return std::regex_search(expression, match, regex) ? match[0] : NO_REGEX_MATCH;
}
