#pragma once

enum
#if  __cplusplus >= 201103L
class
#endif // __cplusplus
token_type
{
	TT_UNKNOWN 			= 1,
	TT_KEYWORD 			= 2,
	TT_LITERAL_NUMERIC 	= 3,
	TT_LITERAL_TEXT 	= 4,
	TT_IDENTIFIER 		= 5,
	TT_DIRECTIVE 		= 6,
	TT_COMMENT 			= 7,
	TT_WHITESPACE 		= 8,
};


#if __cplusplus
extern "C"
#endif // __cplusplus
void handle_token(token_type tokenType, const char *text);

void lex();
