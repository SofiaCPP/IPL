#include "lexer.hpp"

#include "token/token_regexes.hpp"

lexer::lexer()
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
}

vector<token> lexer::tokenize(const string& expression)
{
  m_expression = expression;
  m_expression_point = 0;

  vector<token> tokens;

  while (m_expression_point < m_expression.length())
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
  m_token.m_content = "";

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
}

void lexer::match_keyword_token()
{
  string match = match_regex(KEYWORD_TOKEN_REGEX);

  if (match != NO_TOKEN_REGEX_MATCH && m_keywords.find(match) != m_keywords.end())
  {
    m_token.m_type = m_keywords[match];

    m_expression_point += match.length();
  }
}

void lexer::match_identifier_token()
{
  string match = match_regex(IDENTIFIER_TOKEN_REGEX);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = IDENTIFIER;
    m_token.m_content = match;

    m_expression_point += match.length();
  }
}

void lexer::match_number_token()
{
  string match = match_regex(NUMBER_TOKEN_REGEX);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = NUMBER;
    m_token.m_content = match;

    m_expression_point += match.length();
  }
}

void lexer::match_string_token()
{
  string match = match_regex(SINGLE_QUOTE_STRING_TOKEN_REGEX);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = SINGLE_QUOTE_STRING;
    m_token.m_content = match;

    m_expression_point += match.length();

    return;
  }

  match = match_regex(DOUBLE_QUOTE_STRING_TOKEN_REGEX);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = DOUBLE_QUOTE_STRING;
    m_token.m_content = match;

    m_expression_point += match.length();
  }
}

void lexer::match_symbol_token()
{
  string match = match_regex(SYMBOL_TOKEN_REGEX);

  if (match != NO_TOKEN_REGEX_MATCH)
  {
    m_token.m_type = SYMBOL;
    m_token.m_content = match;

    m_expression_point += match.length();
  }
}

void lexer::match_character_combination_token()
{
  string match = NO_TOKEN_REGEX_MATCH;

  switch (m_expression[m_expression_point])
  {
  case '\n': m_token.m_type = NEWLINE;              break;
  case ';':  m_token.m_type = SEMICOLON;             break;
  case '{':  m_token.m_type = OPEN_CURLY_BRACE;      break;
  case '}':  m_token.m_type = CLOSED_CURLY_BRACE;    break;
  case '(':  m_token.m_type = OPEN_PARENTHESIS;      break;
  case ')':  m_token.m_type = CLOSED_PARENTHESIS;    break;
  case '[':  m_token.m_type = OPEN_SQUARE_BRACKET;   break;
  case ']':  m_token.m_type = CLOSED_SQUARE_BRACKET; break;
  case '!':  m_token.m_type = (match = match_regex(EXCLAMATION_MARK_EQUAL_TOKEN_REGEX)) != NO_TOKEN_REGEX_MATCH ? EXCLAMATION_MARK_EQUAL : EXCLAMATION_MARK; break;
  case '.':  m_token.m_type = DOT;                   break;
  case ',':  m_token.m_type = COMMA;                 break;
  case ':':  m_token.m_type = (match = match_regex(DOUBLE_COLON_TOKEN_REGEX)) != NO_TOKEN_REGEX_MATCH ? DOUBLE_COLON : COLON;                    break;
  case '<':  m_token.m_type = (match = match_regex(LESS_THAN_EQUAL_TOKEN_REGEX)) != NO_TOKEN_REGEX_MATCH ? LESS_THAN_EQUAL : LESS_THAN;          break;
  case '>':  m_token.m_type = (match = match_regex(GREATER_THAN_EQUAL_TOKEN_REGEX)) != NO_TOKEN_REGEX_MATCH ? GREATER_THAN_EQUAL : GREATER_THAN; break;
  case '=':  m_token.m_type = (match = match_regex(DOUBLE_EQUAL_TOKEN_REGEX)) != NO_TOKEN_REGEX_MATCH ? DOUBLE_EQUAL : (match = match_regex(EQUAL_GREATER_THAN_REGEX)) != NO_TOKEN_REGEX_MATCH ? EQUAL_GREATER_THAN : EQUAL; break;
  case '+':  m_token.m_type = (match = match_regex(PLUS_EQUAL_REGEX)) != NO_TOKEN_REGEX_MATCH ? PLUS_EQUAL : PLUS;    break;
  case '-':  m_token.m_type = (match = match_regex(MINUS_EQUAL_REGEX)) != NO_TOKEN_REGEX_MATCH ? MINUS_EQUAL : MINUS; break;
  case '*':  m_token.m_type = (match = match_regex(DOUBLE_ASTERISK_EQUAL_REGEX)) != NO_TOKEN_REGEX_MATCH ? DOUBLE_ASTERISK_EQUAL : (match = match_regex(DOUBLE_ASTERISK_REGEX)) != NO_TOKEN_REGEX_MATCH ? DOUBLE_ASTERISK : (match = match_regex(ASTERISK_EQUAL_REGEX)) != NO_TOKEN_REGEX_MATCH ? ASTERISK_EQUAL : ASTERISK; break;
  case '/':  m_token.m_type = (match = match_regex(SLASH_EQUAL_REGEX)) != NO_TOKEN_REGEX_MATCH ? SLASH_EQUAL : SLASH;          break;
  case '%':  m_token.m_type = (match = match_regex(PROCENT_EQUAL_REGEX)) != NO_TOKEN_REGEX_MATCH ? PROCENT_EQUAL : PROCENT;    break;
  case '&':  m_token.m_type = (match = match_regex(DOUBLE_PIPE_REGEX)) != NO_TOKEN_REGEX_MATCH ? DOUBLE_AMPERSAND : AMPERSAND; break;
  case '|':  m_token.m_type = (match = match_regex(DOUBLE_PIPE_REGEX)) != NO_TOKEN_REGEX_MATCH ? DOUBLE_PIPE : PIPE;           break;
  default:   m_token.m_type = UNRECOGNIZED; break;
  }

  if (m_token.m_type != UNRECOGNIZED)
  {
    if (match != NO_TOKEN_REGEX_MATCH)
      m_expression_point += match.length();
    else
      ++m_expression_point;
  }
}


void lexer::clear_whitespaces()
{
  while (m_expression[m_expression_point] == ' ') ++m_expression_point;
}

string lexer::match_regex(const regex& regex)
{
  regex_match match;
  return std::regex_search(m_expression, match, regex) ? match[0] : NO_TOKEN_REGEX_MATCH;
}
