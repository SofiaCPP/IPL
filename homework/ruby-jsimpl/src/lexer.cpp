#include "lexer.hpp"

#include "token/token_regexes.hpp"

#define NO_TOKEN_REGEX_MATCH string("")

lexer::lexer(const string& expression) :
  m_expression(expression), m_position(0)
{
  m_keywords["if"]     = IF;
  m_keywords["elsif"]  = ELSIF;
  m_keywords["else"]   = ELSE;
  m_keywords["unless"] = UNLESS;
  m_keywords["case"]   = CASE;
  m_keywords["when"]   = WHEN;
  m_keywords["then"]   = THEN;
  m_keywords["for"]    = FOR;
  m_keywords["while"]  = WHILE;
  m_keywords["until"]  = UNTIL;
  m_keywords["do"]     = DO;
  m_keywords["end"]    = END;
  m_keywords["return"] = RETURN;
  m_keywords["and"]    = AND;
  m_keywords["or"]     = OR;
  m_keywords["not"]    = NOT;
  m_keywords["in"]     = IN;
  m_keywords["class"]  = CLASS;
  m_keywords["module"] = MODULE;
  m_keywords["def"]    = DEF;
  m_keywords["nil"]    = NIL;
  m_keywords["true"]   = TRUE;
  m_keywords["false"]  = FALSE;

  m_regexes["KEYWORD_TOKEN_REGEX"] =                regex("^[a-z]+");
  m_regexes["IDENTIFIER_TOKEN_REGEX"] =             regex("^[a-zA-Z_][a-zA-Z0-9_]*");
  m_regexes["NUMBER_TOKEN_REGEX"] =                 regex("^[+-]?([0-9]*[.])?[0-9]+");
  m_regexes["SINGLE_QUOTE_STRING_TOKEN_REGEX"] =    regex("^'.*'");
  m_regexes["DOUBLE_QUOTE_STRING_TOKEN_REGEX"] =    regex("^\".*\"");
  m_regexes["SYMBOL_TOKEN_REGEX"] =                 regex("^:([a-zA-Z_][a-zA-Z0-9_]*|'[a-zA-Z_][a-zA-Z0-9_]*'|\"[a-zA-Z_][a-zA-Z0-9_]*\")");
  m_regexes["EXCLAMATION_MARK_EQUAL_TOKEN_REGEX"] = regex("^!\\=");
  m_regexes["DOUBLE_COLON_TOKEN_REGEX"] =           regex("^::");
  m_regexes["LESS_THAN_EQUAL_TOKEN_REGEX"] =        regex("^<\\=");
  m_regexes["GREATER_THAN_EQUAL_TOKEN_REGEX"] =     regex("^>\\=");
  m_regexes["DOUBLE_EQUAL_TOKEN_REGEX"] =           regex("^\\=\\=");
  m_regexes["EQUAL_GREATER_THAN_REGEX"] =           regex("^\\=>");
  m_regexes["PLUS_EQUAL_REGEX"] =                   regex("^\\+\\=");
  m_regexes["MINUS_EQUAL_REGEX"] =                  regex("^-\\=");
  m_regexes["DOUBLE_ASTERISK_EQUAL_REGEX"] =        regex("^\\*\\*\\=");
  m_regexes["DOUBLE_ASTERISK_REGEX"] =              regex("^\\*\\*");
  m_regexes["ASTERISK_EQUAL_REGEX"] =               regex("^\\*\\=");
  m_regexes["SLASH_EQUAL_REGEX"] =                  regex("^/\\=");
  m_regexes["PROCENT_EQUAL_REGEX"] =                regex("^%\\=");
  m_regexes["DOUBLE_AMPERSAND_REGEX"] =             regex("^&&");
  m_regexes["DOUBLE_PIPE_REGEX"] =                  regex("^\\|\\|");
}

vector<token> lexer::tokenize()
{
  vector<token> tokens;

  while (m_position < m_expression.length())
  {
    clear_whitespaces();

    read_next_token();
    tokens.push_back(m_token);

    clear_whitespaces();
  }

  return tokens;
}

void lexer::read_next_token()
{
  m_token.m_type = UNRECOGNIZED;
  m_token.m_lexeme = "";

  match_keyword_token();
  if (m_token.m_type != UNRECOGNIZED) return;

  match_identifier_token();
  if (m_token.m_type != UNRECOGNIZED) return;

  match_number_token();
  if (m_token.m_type != UNRECOGNIZED) return;

  match_string_token();
  if (m_token.m_type != UNRECOGNIZED) return;

  match_symbol_token();
  if (m_token.m_type != UNRECOGNIZED) return;

  match_character_combination_token();
  if (m_token.m_type != UNRECOGNIZED) return;

  ++m_position;
}

void lexer::match_keyword_token()
{
  string match = match_regex(m_regexes["KEYWORD_TOKEN_REGEX"]);

  if (match != NO_TOKEN_REGEX_MATCH && m_keywords.find(match) != m_keywords.end())
  {
    m_token.m_type = m_keywords[match];

    m_position += match.length();
  }
}

void lexer::match_identifier_token()
{
  string match = match_regex(m_regexes["IDENTIFIER_TOKEN_REGEX"]);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = IDENTIFIER;
    m_token.m_lexeme = match;

    m_position += match.length();
  }
}

void lexer::match_number_token()
{
  string match = match_regex(m_regexes["NUMBER_TOKEN_REGEX"]);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = NUMBER;
    m_token.m_lexeme = match;

    m_position += match.length();
  }
}

void lexer::match_string_token()
{
  string match = match_regex(m_regexes["SINGLE_QUOTE_STRING_TOKEN_REGEX"]);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = SINGLE_QUOTE_STRING;
    m_token.m_lexeme = match;

    m_position += match.length();

    return;
  }

  match = match_regex(m_regexes["DOUBLE_QUOTE_STRING_TOKEN_REGEX"]);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = DOUBLE_QUOTE_STRING;
    m_token.m_lexeme = match;

    m_position += match.length();
  }
}

void lexer::match_symbol_token()
{
  string match = match_regex(m_regexes["SYMBOL_TOKEN_REGEX"]);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = SYMBOL;
    m_token.m_lexeme = match;

    m_position += match.length();
  }
}

void lexer::match_character_combination_token()
{
  string match = NO_TOKEN_REGEX_MATCH;

  switch (m_expression[m_position])
  {
  case '\n': m_token.m_type = NEWLINE;               break;
  case ';':  m_token.m_type = SEMICOLON;             break;
  case '{':  m_token.m_type = OPEN_CURLY_BRACE;      break;
  case '}':  m_token.m_type = CLOSED_CURLY_BRACE;    break;
  case '(':  m_token.m_type = OPEN_PARENTHESIS;      break;
  case ')':  m_token.m_type = CLOSED_PARENTHESIS;    break;
  case '[':  m_token.m_type = OPEN_SQUARE_BRACKET;   break;
  case ']':  m_token.m_type = CLOSED_SQUARE_BRACKET; break;
  case '!':  m_token.m_type = (match = match_regex(m_regexes["EXCLAMATION_MARK_EQUAL_TOKEN_REGEX"])) != NO_TOKEN_REGEX_MATCH ? EXCLAMATION_MARK_EQUAL : EXCLAMATION_MARK; break;
  case '.':  m_token.m_type = DOT;                   break;
  case ',':  m_token.m_type = COMMA;                 break;
  case ':':  m_token.m_type = (match = match_regex(m_regexes["DOUBLE_COLON_TOKEN_REGEX"])) != NO_TOKEN_REGEX_MATCH ? DOUBLE_COLON : COLON;                    break;
  case '<':  m_token.m_type = (match = match_regex(m_regexes["LESS_THAN_EQUAL_TOKEN_REGEX"])) != NO_TOKEN_REGEX_MATCH ? LESS_THAN_EQUAL : LESS_THAN;          break;
  case '>':  m_token.m_type = (match = match_regex(m_regexes["GREATER_THAN_EQUAL_TOKEN_REGEX"])) != NO_TOKEN_REGEX_MATCH ? GREATER_THAN_EQUAL : GREATER_THAN; break;
  case '=':  m_token.m_type = (match = match_regex(m_regexes["DOUBLE_EQUAL_TOKEN_REGEX"])) != NO_TOKEN_REGEX_MATCH ? DOUBLE_EQUAL : (match = match_regex(m_regexes["EQUAL_GREATER_THAN_REGEX"])) != NO_TOKEN_REGEX_MATCH ? EQUAL_GREATER_THAN : EQUAL; break;
  case '+':  m_token.m_type = (match = match_regex(m_regexes["PLUS_EQUAL_REGEX"])) != NO_TOKEN_REGEX_MATCH ? PLUS_EQUAL : PLUS;    break;
  case '-':  m_token.m_type = (match = match_regex(m_regexes["MINUS_EQUAL_REGEX"])) != NO_TOKEN_REGEX_MATCH ? MINUS_EQUAL : MINUS; break;
  case '*':  m_token.m_type = (match = match_regex(m_regexes["DOUBLE_ASTERISK_EQUAL_REGEX"])) != NO_TOKEN_REGEX_MATCH ? DOUBLE_ASTERISK_EQUAL : (match = match_regex(m_regexes["DOUBLE_ASTERISK_REGEX"])) != NO_TOKEN_REGEX_MATCH ? DOUBLE_ASTERISK : (match = match_regex(m_regexes["ASTERISK_EQUAL_REGEX"])) != NO_TOKEN_REGEX_MATCH ? ASTERISK_EQUAL : ASTERISK; break;
  case '/':  m_token.m_type = (match = match_regex(m_regexes["SLASH_EQUAL_REGEX"])) != NO_TOKEN_REGEX_MATCH ? SLASH_EQUAL : SLASH;          break;
  case '%':  m_token.m_type = (match = match_regex(m_regexes["PROCENT_EQUAL_REGEX"])) != NO_TOKEN_REGEX_MATCH ? PROCENT_EQUAL : PROCENT;    break;
  case '&':  m_token.m_type = (match = match_regex(m_regexes["DOUBLE_AMPERSAND_REGEX"])) != NO_TOKEN_REGEX_MATCH ? DOUBLE_AMPERSAND : AMPERSAND; break;
  case '|':  m_token.m_type = (match = match_regex(m_regexes["DOUBLE_PIPE_REGEX"])) != NO_TOKEN_REGEX_MATCH ? DOUBLE_PIPE : PIPE;           break;
  default:   m_token.m_type = UNRECOGNIZED; break;
  }

  if (m_token.m_type != UNRECOGNIZED)
  {
    if (match != NO_TOKEN_REGEX_MATCH)
      m_position += match.length();
    else
      ++m_position;
  }
}


void lexer::clear_whitespaces()
{
  while (m_expression[m_position] == ' ') ++m_position;
}

string lexer::match_regex(const regex& regex)
{
  regex_match match;
  return std::regex_search(m_expression.c_str() + m_position, match, regex) ? match[0] : NO_TOKEN_REGEX_MATCH;
}
