
%{
#include "stack.h"
#include <stdio.h>
#include "parser.tab.h"

int g_current_line_indent = 0;
int g_is_fake_outdent_symbol = 0;
int current_line = 0;
int is_read_indent = 0;
int is_preveous_unindent = 0;

static const unsigned int TAB_WIDTH = 2;

#define YY_USER_INIT { \
        push(0); \
        BEGIN(initial); \
    }

int yycolumn = 1;
void set_yycolumn(int val) {
    yycolumn = val;
    yylloc.first_column = yycolumn;
    yylloc.last_column = yycolumn + yyleng - 1;
}

#define YY_USER_ACTION { \
    yylloc.first_line = yylloc.last_line = yylineno; \
    set_yycolumn(yycolumn); \
    yycolumn += yyleng; \
}

char* fixed_unindent(char* text){
  char* add_new_line = "";
  if(is_preveous_unindent){
    add_new_line ="<br>";
    is_preveous_unindent = 0;
  }
  return concatenate(add_new_line, text);
}

%}

 /* This is a sub-parser (state) for indentation-sensitive scoping */
%x initial
%x indent
%s normal

 /* %option 8bit reentrant bison-bridge */
%option warn
%option nodefault
%option yylineno
%option noyywrap


digit         [0-9]
number [1-9]{digit}*
fnumber {digit}+"."{digit}*
letter        [a-zA-Z_]
stringdq \"([^\"]|(\\\"))*\"
stringsq \'([^\']|(\\\'))*\'

 /*%option debug*/
%%
    int indent_caller = normal;


<*>\n { set_yycolumn(0); yylineno--; REJECT; }


<initial>.  { set_yycolumn(yycolumn-1); indent_caller = normal; yyless(0); BEGIN(indent); }
<initial>\n { indent_caller = normal; yyless(0); BEGIN(indent); }

 /* The following are the rules that keep track of indentation. */
<indent>" "     { g_current_line_indent++; }
<indent>\t      { g_current_line_indent = (g_current_line_indent + TAB_WIDTH) & ~(TAB_WIDTH-1); }
<indent>\n      { g_current_line_indent = 0; /* ignoring blank line */ }
<indent><<EOF>> {
                    if(peek() != 0) {
                        pop();

                        if(g_current_line_indent != peek()) {
                            unput('\n');
                            for(size_t i = 0 ; i < peek() ; ++i) {
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

<indent>.       {
                  //  if(!g_is_fake_outdent_symbol) {
                        unput(*yytext);
                  //  }
                    set_yycolumn(yycolumn-1);
                    g_is_fake_outdent_symbol = 0;


                    if(g_current_line_indent > peek()) {
                        push(g_current_line_indent);
                        BEGIN(indent_caller);
                        return INDENT;
                    } else if(g_current_line_indent < peek()) {
                        pop();

                        if(g_current_line_indent != peek()) {
                            for(size_t i = 0 ; i < g_current_line_indent ; ++i) {
                                unput(' ');
                            }
                            unput('\n');

                            unput('$');

                            g_is_fake_outdent_symbol = 1;
                            for(size_t i = 0 ; i < peek() ; ++i) {
                                unput(' ');
                            }
                            unput('\n');
                        } else {

                            BEGIN(indent_caller);
                        }
                        is_preveous_unindent = 1;
                        return UNINDENT;
                    } else {
                        BEGIN(indent_caller);
                    }
                }
<normal>" "      { yylval.lexeme = " ";}
<normal>\n    { g_current_line_indent = 0; indent_caller = YY_START; BEGIN(indent); yylval.lexeme = "<br>"; return NEWLINE; }

<normal>{number}|{fnumber}   { yylval.lexeme = fixed_unindent(yytext); return NUMBER; }
<normal>{digit}              { yylval.lexeme = fixed_unindent(yytext); return NUMBER;}
<normal>{stringsq}           { yylval.lexeme = fixed_unindent(yytext); return STRING;}
<normal>{stringdq}           { yylval.lexeme = fixed_unindent(yytext); return STRING;}

<normal>"+"                  { yylval.lexeme = fixed_unindent(yytext); return PLUS;       }
<normal>"-"                  { yylval.lexeme = fixed_unindent(yytext); return MINUS;      }
<normal>"*"                  { yylval.lexeme = fixed_unindent(yytext); return STAR;       }
<normal>"/"                  { yylval.lexeme = fixed_unindent(yytext); return SLASH;      }
<normal>"//"		             { yylval.lexeme = fixed_unindent(yytext); return DOUBLESLASH;}
<normal>"%"		               { yylval.lexeme = fixed_unindent(yytext); return PERCENT;    }
<normal>"**"		             { yylval.lexeme = fixed_unindent(yytext); return DOUBLESTAR; }
<normal>">"		               { yylval.lexeme = fixed_unindent(yytext); return BIGGER;     }
<normal>"<"		               { yylval.lexeme = fixed_unindent(yytext); return LESS;	  }
<normal>">="		             { yylval.lexeme = fixed_unindent(yytext); return BIGGEREQL;  }
<normal>"<="		             { yylval.lexeme = fixed_unindent(yytext); return LESSEQL;    }
<normal>"=="		             { yylval.lexeme = fixed_unindent(yytext); return EQL;	  }
<normal>"!="		             { yylval.lexeme = fixed_unindent(yytext); return NOTEQL;	  }


<normal>"&"		               { yylval.lexeme = fixed_unindent(yytext); return BOOLAND;    }
<normal>"|"		               { yylval.lexeme = fixed_unindent(yytext); return BOOLOR;     }
<normal>"~"		               { yylval.lexeme = fixed_unindent(yytext); return TILDA;      }
<normal>"^"                  { yylval.lexeme = fixed_unindent(yytext); return HAT;        }
<normal>">>"                 { yylval.lexeme = fixed_unindent(yytext); return SHIFTRIGHT; }
<normal>"<<"                 { yylval.lexeme = fixed_unindent(yytext); return SHIFTLESS;  }


<normal>"="		               { yylval.lexeme = fixed_unindent(yytext); return ASSIGN;     }
<normal>"+="                 { yylval.lexeme = fixed_unindent(yytext); return PLUSASSIGN; }
<normal>"-="                 { yylval.lexeme = fixed_unindent(yytext); return MINUSASSIGN;}
<normal>"*="                 { yylval.lexeme = fixed_unindent(yytext); return STARASSIGN; }
<normal>"/="                 { yylval.lexeme = fixed_unindent(yytext); return SLASHASSIGN;}
<normal>"%="                 { yylval.lexeme = fixed_unindent(yytext); return PERCENTASSIGN;}
<normal>"**="                { yylval.lexeme = fixed_unindent(yytext); return DOUBLESTARASSIGN;}
<normal>"//="                { yylval.lexeme = fixed_unindent(yytext); return DOUBLESLASHASSIGN;}
<normal>"^="                 { yylval.lexeme = fixed_unindent(yytext); return HATASSIGN;}
<normal>"&="                 { yylval.lexeme = fixed_unindent(yytext); return BOOLANDASSIGN;}
<normal>"|="                 { yylval.lexeme = fixed_unindent(yytext); return BOOLORASSIGN;}
<normal>">>="                { yylval.lexeme = fixed_unindent(yytext); return SHIFTRIGHTASSIGN;}
<normal>"<<="                { yylval.lexeme = fixed_unindent(yytext); return SHIFTLEFTASSIGN;}
<normal>"@="                 { yylval.lexeme = fixed_unindent(yytext); return ATASSIGN; }

<normal>"@"                  { yylval.lexeme = fixed_unindent(yytext); return AT;}
<normal>"("                  { yylval.lexeme = fixed_unindent(yytext); return LPAREN;     }
<normal>")"                  { yylval.lexeme = fixed_unindent(yytext); return RPAREN;     }
<normal>"["                  { yylval.lexeme = fixed_unindent(yytext); return LSQPAREN;}
<normal>"]"                  { yylval.lexeme = fixed_unindent(yytext); return RSQPAREN;}
<normal>"{"                  { yylval.lexeme = fixed_unindent(yytext); return LCURPAREN;}
<normal>"}"                  { yylval.lexeme = fixed_unindent(yytext); return RCURPAREN;}
<normal>";"                  { yylval.lexeme = fixed_unindent(yytext); return SEMICOLON;  }
<normal>":"                  { yylval.lexeme = fixed_unindent(yytext); return COLON;      }
<normal>","                  { yylval.lexeme = fixed_unindent(yytext); return COMMA;      }
<normal>"."                  { yylval.lexeme = fixed_unindent(yytext); return DOT;    	  }
<normal>"..."                { yylval.lexeme = fixed_unindent(yytext); return THREEDOTS;}

<normal>"$"                  { return DOLAR; }

<normal>"async"              { yylval.lexeme = fixed_unindent(yytext); return ASYNC;}
<normal>"def"                { yylval.lexeme = fixed_unindent(yytext); return DEF; }
<normal>"and"                { yylval.lexeme = fixed_unindent(yytext); return AND;}
<normal>"or"                 { yylval.lexeme = fixed_unindent(yytext); return OR;}
<normal>"not"                { yylval.lexeme = fixed_unindent(yytext); return NOT;}
<normal>"in"                 { yylval.lexeme = fixed_unindent(yytext); return IN;}
<normal>"not in"             { yylval.lexeme = fixed_unindent(yytext); return NOTIN;}
<normal>"is"                 { yylval.lexeme = fixed_unindent(yytext); return IS;}
<normal>"is not"             { yylval.lexeme = fixed_unindent(yytext); return ISNOT;}
<normal>"await"              { yylval.lexeme = fixed_unindent(yytext); return AWAIT;}
<normal>"None"               { yylval.lexeme = fixed_unindent(yytext); return NONE;}
<normal>"True"               { yylval.lexeme = fixed_unindent(yytext); return TRUE;}
<normal>"False"              { yylval.lexeme = fixed_unindent(yytext); return FALSE;}
<normal>"class"              { yylval.lexeme = fixed_unindent(yytext); return CLASS;}
<normal>"for"                { yylval.lexeme = fixed_unindent(yytext); return FOR;}
<normal>"if"                 { yylval.lexeme = fixed_unindent(yytext); return IF;}
<normal>"elif"               { yylval.lexeme = fixed_unindent(yytext); return ELIF;}
<normal>"else"               { yylval.lexeme = fixed_unindent(yytext); return ELSE;}
<normal>"except"             { yylval.lexeme = fixed_unindent(yytext); return EXCEPT;}
<normal>"from"               { yylval.lexeme = fixed_unindent(yytext); return FROM;}
<normal>"finally"            { yylval.lexeme = fixed_unindent(yytext); return FINALLY;}
<normal>"lambda"             { yylval.lexeme = fixed_unindent(yytext); return LAMBDA;}
<normal>"yield"              { yylval.lexeme = fixed_unindent(yytext); return YIELD;}
<normal>"as"                 { yylval.lexeme = fixed_unindent(yytext); return AS;}
<normal>"assert"             { yylval.lexeme = fixed_unindent(yytext); return ASSERT;}
<normal>"break"              { yylval.lexeme = fixed_unindent(yytext); return BREAK;}
<normal>"continue"           { yylval.lexeme = fixed_unindent(yytext); return CONTINUE;}
<normal>"del"                { yylval.lexeme = fixed_unindent(yytext); return DEL;}
<normal>"global"             { yylval.lexeme = fixed_unindent(yytext); return GLOBAL;}
<normal>"import"             { yylval.lexeme = fixed_unindent(yytext); return IMPORT;}
<normal>"nonlocal"           { yylval.lexeme = fixed_unindent(yytext); return NONLOCAL;}
<normal>"pass"               { yylval.lexeme = fixed_unindent(yytext); return PASS;}
<normal>"raise"              { yylval.lexeme = fixed_unindent(yytext); return RAISE;}
<normal>"return"             { yylval.lexeme = fixed_unindent(yytext); return RETURN;}
<normal>"try"                { yylval.lexeme = fixed_unindent(yytext); return TRY;}
<normal>"while"              { yylval.lexeme = fixed_unindent(yytext); return WHILE;}
<normal>"with"               { yylval.lexeme = fixed_unindent(yytext); return WITH;}

<normal>{letter}({letter}|{digit})* {
                      yylval.lexeme = fixed_unindent(yytext); return NAME; }
%%
