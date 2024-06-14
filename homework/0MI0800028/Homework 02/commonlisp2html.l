%{
#include "commonlisp2html.tab.h"
%}

DIGIT [0-9]
LEFTBRACKET \(
RIGHTBRACKET \)

STR   \"([^\"]|\\\")*([^\\]|\\\\)\"
NUMBERS [^A-z](\+|-)?{DIGIT}+(\.{DIGIT}+(e{DIGIT}+|d{DIGIT}+|e-{DIGIT}+)?|\/{DIGIT}+)?

COMMENT ;.*$
PARAMETER_TYPE &[0-9A-z]+
KEYWORD :[0-9A-z]+
CONSTANTS T|NIL|pi|t

SPECIAL_FORM block|function|if|let|quote|return-from|defun|demacro

/* Functions */
F_ARITHMETIC "+ "|"- "|"* "|\/|1\+|1-|conjugate|gcd|lcm
F_NUM_COMPARISON =|\/=|<|>|<=|>=|max|min
F_TRIGONOMETRIC sin|cos|tan|cis|asin|acos|atan|sinh|cosh|tanh|asinh|acosh|atanh
F_OTHER_MATH abs|sqrt|phase|signum
F_LOGICAL not
F_LISTS list
F_OTHER format

/* Macros */
M_ARITHMETIC incf|decf
M_LOGICAL and|or
M_CONDITIONAL when|unless|cond|case|typecase
M_DECLARE defvar|defparameter|defconstant|defun

/* Not in spec, but commonly followed */
GLOBAL_VAR \*([0-9A-z.?-_])+\*
CONST_VAR \+([0-9A-z.?-_])+\+

%%

{LEFTBRACKET}    { return LBRACKET; }
{RIGHTBRACKET}   { return RBRACKET; }
{STR}            { yylval = strdup(yytext); return STR; }
{NUMBERS}        { yylval = strdup(yytext); return NUM; }
{COMMENT}        { yylval = strdup(yytext); return COMMENT;}
{PARAMETER_TYPE} { yylval = strdup(yytext); return PARAMETER_TYPE; }
{KEYWORD}        { yylval = strdup(yytext); return KEYWORD; }
{CONSTANTS}      { yylval = strdup(yytext); return CONSTANT; }

{SPECIAL_FORM} { yylval = strdup(yytext); return SPECIAL_FORM; }
{F_ARITHMETIC}|{F_NUM_COMPARISON}|{F_TRIGONOMETRIC}|{F_TRIGONOMETRIC}|{F_OTHER_MATH}|{F_LOGICAL}|{F_LISTS}|{F_OTHER} { yylval = strdup(yytext); return FUNCTION; }
{M_ARITHMETIC}|{M_LOGICAL}|{M_CONDITIONAL}|{M_DECLARE} { yylval = strdup(yytext); return MACRO; }

{GLOBAL_VAR}|{CONST_VAR} { yylval = strdup(yytext); return VARIABLE; }

\n { return NEWLINE; }

	/* flex catches the longest possibility, so if some variable name contains a known word (function, macro, etc.), this will prevent it from highlighting */
[A-z!-'*-@]+|" "|\t { yylval = strdup(yytext); return OTHER; }


%%
