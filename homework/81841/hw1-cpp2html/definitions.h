#pragma once
#define TT_KEYWORD 1
#define TT_LITERAL_NUMERIC 2
#define TT_LITERAL_TEXT 3
#define TT_IDENTIFIER 4
#define TT_DIRECTIVE 997
#define TT_COMMENT 998
#define TT_WHITESPACE 999
#define TT_UNKNOWN 1000


#if __cplusplus
extern "C"
#endif // __cplusplus
void handle_token(int tokenType, const char *text);

void lex();
