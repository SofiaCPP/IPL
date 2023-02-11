%{

%}

DIGIT [0-9]
BRACKET \(|\)

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

{BRACKET} { printf("<span class=\"bracket\">%s</span>", yytext); }
{STR} { printf("<span class=\"string\">%s</span>", yytext); }
{NUMBERS} { printf("<span class=\"number\">%s</span>", yytext); }
{COMMENT} { printf("<span class=\"comment\">%s</span>", yytext); }
{PARAMETER_TYPE} { printf("<span class=\"parameter-type\">%s</span>", yytext); }
{KEYWORD} { printf("<span class=\"keyword\">%s</span>", yytext); }
{CONSTANTS} { printf("<span class=\"constant\">%s</span>", yytext); }

{SPECIAL_FORM} { printf("<span class=\"special-operator\">%s</span>", yytext); }
{F_ARITHMETIC}|{F_NUM_COMPARISON}|{F_TRIGONOMETRIC}|{F_TRIGONOMETRIC}|{F_OTHER_MATH}|{F_LOGICAL}|{F_LISTS}|{F_OTHER} { printf("<span class=\"function\">%s</span>", yytext); }
{M_ARITHMETIC}|{M_LOGICAL}|{M_CONDITIONAL}|{M_DECLARE} { printf("<span class=\"macro\">%s</span>", yytext); }

{GLOBAL_VAR}|{CONST_VAR} { printf("<span class=\"variable\">%s</span>", yytext); }

	/* flex catches the longest possibility, so if some variable name contains a known word (function, macro, etc.), this will prevent it from highlighting */
[A-z]+ { printf("%s", yytext); }

%%
