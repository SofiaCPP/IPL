#ifndef TOKEN_TOKEN_REGEXES_HPP
#define TOKEN_TOKEN_REGEXES_HPP

#define KEYWORD_TOKEN_REGEX                regex("^[a-z]+")
#define IDENTIFIER_TOKEN_REGEX             regex("^[a-zA-Z_][a-zA-Z0-9_]*")
#define NUMBER_TOKEN_REGEX                 regex("^[+-]?([0-9]*[.])?[0-9]+")
#define SINGLE_QUOTE_STRING_TOKEN_REGEX    regex("^'.*'")
#define DOUBLE_QUOTE_STRING_TOKEN_REGEX    regex("^\".*\"")
#define SYMBOL_TOKEN_REGEX                 regex("^:([a-zA-Z_][a-zA-Z0-9_]*|'.*'|\".*\")")
#define EXCLAMATION_MARK_EQUAL_TOKEN_REGEX regex("^!\\=")
#define DOUBLE_COLON_TOKEN_REGEX           regex("^::")
#define LESS_THAN_EQUAL_TOKEN_REGEX        regex("^<\\=")
#define GREATER_THAN_EQUAL_TOKEN_REGEX     regex("^>\\=")
#define DOUBLE_EQUAL_TOKEN_REGEX           regex("^\\=\\=")
#define EQUAL_GREATER_THAN_REGEX           regex("^\\=>")
#define PLUS_EQUAL_REGEX                   regex("^+\\=")
#define MINUS_EQUAL_REGEX                  regex("^-\\=")
#define DOUBLE_ASTERISK_EQUAL_REGEX        regex("^\\*\\*\\=")
#define DOUBLE_ASTERISK_REGEX              regex("^\\*\\*")
#define ASTERISK_EQUAL_REGEX               regex("^\\*\\=")
#define SLASH_EQUAL_REGEX                  regex("^/\\=")
#define PROCENT_EQUAL_REGEX                regex("^%\\=")
#define DOUBLE_AMPERSAND_REGEX             regex("^&&")
#define DOUBLE_PIPE_REGEX                  regex("^||")
#define NO_TOKEN_REGEX_MATCH               string("")

#endif
