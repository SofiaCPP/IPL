%{
/* need this for the call to atof() below */
#include <math.h>
#include "parser.tab.h"
#include "stack.h"
#define YY_NO_UNISTD_H
static const unsigned int TAB_WIDTH = 2;

#define YY_USER_INIT { push(0); BEGIN(initial); }
int current_line = 0;
int current_line_indent = 0;
int indent_level = 0;
int is_fake_sym = 0;
%}

digit         [0-9]
number [1-9]{digit}*
fnumber {digit}+"."{digit}*
letter        [a-zA-Z]
string ["]{3}(["]{0,2}([^\\"]|\\(.|\n)))*["]{3}

%x initial
%x indent
%s normal

%%
  int indent_caller = normal;
<*>"\n" { REJECT; }

<initial>.  { unput(*yytext); indent_caller = normal; BEGIN(indent); }
<initial>\n { indent_caller = normal; BEGIN(indent); }

<indent>" "          { current_line_indent++;}
<indent>"\t"         { current_line_indent = (current_line_indent+2) & ~1;} /* one tab is 2 spaces*/
<indent>"\n"         { current_line_indent = 0; return NEWLINE;}
<indent><<EOF>>      {
                        if(peek() != 0){
                          pop();
                          if(current_line_indent != peek()){
                            unput('\n');
                            for(int i=0; i< peek(); i++){
                              unput(' ');
                            }
                          } else {
                            BEGIN(indent_caller);
                          }
                          return UNINDENT;
                        } else {
                          yyterminate();
                        }
                      }
<indent>.        {
                  if(!is_fake_sym){
                     unput(*yytext);
                  }
                  is_fake_sym = 0;
                   if (current_line_indent > peek()) {
                       push(current_line_indent);
                       BEGIN(indent_caller);
                       return INDENT;
                   } else if (current_line_indent < peek()) {
                       pop();
                       if(current_line_indent != peek()){
                         for(int i=0; i< current_line_indent; i++){
                           unput(' ');
                         }
                           unput('\n');

                         unput('.');
                         is_fake_sym = 1;
                         for(int i=0; i < peek(); i++){
                           unput(' ');
                           is_fake_sym = 1;
                         }
                         unput('\n');
                         is_fake_sym = 1;
                       } else {
                         BEGIN(indent_caller);
                       }
                       return UNINDENT;
                   } else {
                       BEGIN(indent_caller);
                   }
                 }
<normal>"\n"     { current_line_indent = 0; indent_caller = YY_START; BEGIN(indent); return NEWLINE;}

<normal>{number}|{fnumber}   { yylval.Number = atof(yytext); return NUMBER; }
<normal>{digit}              { yylval.Number = atof(yytext); return NUMBER;}
<normal>{string}             { strcpy(yylval.Lexeme, strdup(yytext)); return STRING;}

<normal>"+"                  { return PLUS;       }
<normal>"-"                  { return MINUS;      }
<normal>"*"                  { return STAR;       }
<normal>"/"                  { return SLASH;      }
<normal>"//"		             { return DOUBLESLASH;}
<normal>"%"		               { return PERCENT;    }
<normal>"**"		             { return DOUBLESTAR; }
<normal>">"		               { return BIGGER;     }
<normal>"<"		               { return LESS;	  }
<normal>">="		             { return BIGGEREQL;  }
<normal>"<="		             { return LESSEQL;    }
<normal>"=="		             { return EQL;	  }
<normal>"!="		             { return NOTEQL;	  }


<normal>"&"		               { return BOOLAND;    }
<normal>"|"		               { return BOOLOR;     }
<normal>"~"		               { return TILDA;      }
<normal>"^"                  { return HAT;        }
<normal>">>"                 { return SHIFTRIGHT; }
<normal>"<<"                 { return SHIFTLESS;  }


<normal>"="		               { return ASSIGN;     }
<normal>"+="                 { return PLUSASSIGN; }
<normal>"-="                 { return MINUSASSIGN;}
<normal>"*="                 { return STARASSIGN; }
<normal>"/="                 { return SLASHASSIGN;}
<normal>"%="                 { return PERCENTASSIGN;}
<normal>"**="                { return DOUBLESTARASSIGN;}
<normal>"//="                { return DOUBLESLASHASSIGN;}
<normal>"^="                 { return HATASSIGN;}
<normal>"&="                 { return BOOLANDASSIGN;}
<normal>"|="                 { return BOOLORASSIGN;}
<normal>">>="                { return SHIFTRIGHTASSIGN;}
<normal>"<<="                { return SHIFTLEFTASSIGN;}
<normal>"@="                 { return ATASSIGN; }

<normal>"@"                  { return AT;}
<normal>"("                  { return LPAREN;     }
<normal>")"                  { return RPAREN;     }
<normal>"["                  { return LSQPAREN;}
<normal>"]"                  { return RSQPAREN;}
<normal>"{"                  { return LCURPAREN;}
<normal>"}"                  { return RCURPAREN;}
<normal>";"                  { return SEMICOLON;  }
<normal>":"                  { return COLON;      }
<normal>","                  { return COMMA;      }
<normal>"."                  { return DOT;    	  }
<normal>"..."                { return THREEDOTS;}

<normal>"async"              { return ASYNC;}
<normal>"def"                { return DEF; }
<normal>"and"                { return AND;}
<normal>"or"                 { return OR;}
<normal>"not"                { return NOT;}
<normal>"in"                 { return IN;}
<normal>"not in"             { return NOTIN;}
<normal>"is"                 { return IS;}
<normal>"is not"             { return ISNOT;}
<normal>"await"              { return AWAIT;}
<normal>"None"               { return NONE;}
<normal>"True"               { return TRUE;}
<normal>"False"              { return FALSE;}
<normal>"class"              { return CLASS;}
<normal>"for"                { return FOR;}
<normal>"if"                 { return IF;}
<normal>"elif"               { return ELIF;}
<normal>"else"               { return ELSE;}
<normal>"except"             { return EXCEPT;}
<normal>"from"               { return FROM;}
<normal>"finally"            { return FINALLY;}
<normal>"lambda"             { return LAMBDA;}
<normal>"yield"              { return YIELD;}
<normal>"as"                 { return AS;}
<normal>"assert"             { return ASSERT;}
<normal>"break"              { return BREAK;}
<normal>"continue"           { return CONTINUE;}
<normal>"del"                { return DEL;}
<normal>"global"             { return GLOBAL;}
<normal>"import"             { return IMPORT;}
<normal>"nonlocal"           { return NONLOCAL;}
<normal>"pass"               { return PASS;}
<normal>"raise"              { return RAISE;}
<normal>"return"             { return RETURN;}
<normal>"try"                { return TRY;}
<normal>"while"              { return WHILE;}
<normal>"with"               { return WITH;}

<normal>{letter}({letter}|{digit})* {
                       strcpy(yylval.Lexeme, strdup(yytext));
                       return NAME;      }
