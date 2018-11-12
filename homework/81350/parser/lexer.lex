%{
/* need this for the call to atof() below */
#include <math.h>
#include "parser.tab.h"
#define YY_NO_UNISTD_H
int current_line = 0;
%}

digit         [0-9]
number [1-9]{digit}*
fnumber {digit}+"."{digit}*
letter        [a-zA-Z]

%%

{number}|{fnumber} { yylval.Number = atof(yytext); return NUMBER; }
"+"                  { return PLUS;       }
"-"                  { return MINUS;      }
"*"                  { return STAR;       }
"/"                  { return SLASH;      }
"//"		             { return DOUBLESLASH;}
"%"		               { return PERCENT;    }
"**"		             { return DOUBLESTAR; }
">"		               { return BIGGER;     }
"<"		               { return LESS;	  }
">="		             { return BIGGEREQL;  }
"<="		             { return LESSEQL;    }
"=="		             { return EQL;	  }
"!="		             { return NOTEQL;	  }


"&"		               { return BOOLAND;    }
"|"		               { return BOOLOR;     }
"~"		               { return TILDA;      }
"^"                  { return HAT;        }
">>"                 { return SHIFTRIGHT; }
"<<"                 { return SHIFTLESS;  }


"="		               { return ASSIGN;     }
"+="                 { return PLUSASSIGN; }
"-="                 { return MINUSASSIGN;}
"*="                 { return STARASSIGN; }
"/="                 { return SLASHASSIGN;}
"%="                 { return PERCENTASSIGN;}
"**="                { return DOUBLESTARASSIGN;}
"//="                { return DOUBLESLASHASSIGN;}
"^="                 { return HATASSIGN;}
"&="                 { return BOOLANDASSIGN;}
"|="                 { return BOOLORASSIGN;}
">>="                { return SHIFTRIGHTASSIGN;}
"<<="                { return SHIFTLEFTASSIGN;}


"("                  { return LPAREN;     }
")"                  { return RPAREN;     }
";"                  { return SEMICOLON;  }
","                  { return COMMA;      }
"."                  { return DOT;    	  }



{letter}({letter}|{digit})* {
                       strcpy(yylval.Lexeme, strdup(yytext));
                       return IDENT;      }
