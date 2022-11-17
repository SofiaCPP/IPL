%{
	#include "lua_parser.tab.h"
%}

number    [0-9]
id       [a-zA-Z_0-9]*
keyword and|break|false|goto|in|local|nil|not|or|repeat|return|then|true|until|do
operator "+"|"-"|"*"|"/"|"%"|"^"|"#"|"=="|"~="|"<="|">="|"<"|">"|"="|"("|")"|"{"|"}"|"["|"]"|"::"|";"|":"|","|"."|".."|"..."|"<<"|">>"|"&"|"|"|"//"|"~"|"'"|"--"|"?"
open_bracket "("
closing_bracket ")"
func function
statement_keyword while|for|if|else|elseif
compos_end_key end

%%

{number}+    {
            printf("<span class=\"number\">%s</span>", yytext); yylval = strdup(yytext);
						/* scanf(yytext, "$s", yylval.number); */
						return (NUMBER);
            }
{keyword}     { printf("<span class=\"keyword\">%s</span>", yytext);
								yylval = strdup(yytext);
								return (KEYWORD);
							}

{statement_keyword} { yylval = strdup(yytext);
								   return (STATEMENT_KEYWORD);
								 }

{compos_end_key} { yylval = strdup(yytext);
								   return (COMPOS_END_KEY);
								 }

{func} { yylval = strdup(yytext);
				 return (FUNK);
			 }

(\".*\") printf("<span class=\"string\">%s</span>", yytext);

{id}    { printf("<span class=\"identifier\">%s</span>", yytext);yylval = strdup(yytext);
					return (ID);
				}

(--.*) printf("<span class=\"comment\">%s</span>", yytext);

{operator}  { printf("<span class=\"operator\">%s</span>", yytext);yylval = strdup(yytext);
							return (OPERATOR);
						}
[ \t\n]+          printf("%s", yytext);

.           printf( "Unrecognized character: %s\n", yytext );

%%

int yywrap()
{
    return 1;
}
int yyerror()
{
	return 1;
}
