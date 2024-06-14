%e  1019
%p  2807
%n  371
%k  284
%a  1213
%o  1117

O   [0-7]
D   [0-9]
NZ  [1-9]
L   [a-zA-Z_]
A   [a-zA-Z_0-9]
H   [a-fA-F0-9]
HP  (0[xX])
E   ([Ee][+-]?{D}+)
P   ([Pp][+-]?{D}+)
FS  (f|F|l|L)
IS  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
CP  (u|U|L)
SP  (u8|u|U|L)
ES  (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
WS  [ \t\v\f]
NL  [\n]

%{
#include <stdio.h>
#include <unistd.h>

#include "y.tab.h"

int comment_flag = 0;
#define PRINT_DIV(color, text) do{ printf("<font color=\"%s\">%s</font>", (comment_flag?"green":(color)), (text)); }while(0);

extern void yyerror(const char *);  /* prints grammar violation message */

extern int sym_type(const char *);  /* returns type from symbol table */

#define sym_type(identifier) IDENTIFIER /* with no symbol table, fake it */

static void comment(void);
static int check_type(void);
%}

%%
"/*"                    { comment_flag = 1; PRINT_DIV("green", yytext); }
"//".*                  { comment_flag = 1; PRINT_DIV("green", yytext); }
"*/"                    { PRINT_DIV("green", yytext); comment_flag = 0; }

"#"{L}*                 { PRINT_DIV("pink", yytext); }

"auto"					{ PRINT_DIV("blue", yytext); }
"break"					{ PRINT_DIV("blue", yytext); }
"case"					{ PRINT_DIV("blue", yytext); }
"char"					{ PRINT_DIV("blue", yytext); }
"const"					{ PRINT_DIV("blue", yytext); }
"continue"				{ PRINT_DIV("blue", yytext); }
"default"				{ PRINT_DIV("blue", yytext); }
"do"					{ PRINT_DIV("blue", yytext); }
"double"				{ PRINT_DIV("blue", yytext); }
"else"					{ PRINT_DIV("blue", yytext); }
"enum"					{ PRINT_DIV("blue", yytext); }
"extern"				{ PRINT_DIV("blue", yytext); }
"float"					{ PRINT_DIV("blue", yytext); }
"for"					{ PRINT_DIV("blue", yytext); }
"goto"					{ PRINT_DIV("blue", yytext); }
"if"					{ PRINT_DIV("blue", yytext); }
"inline"				{ PRINT_DIV("blue", yytext); }
"int"					{ PRINT_DIV("blue", yytext); }
"long"					{ PRINT_DIV("blue", yytext); }
"register"				{ PRINT_DIV("blue", yytext); }
"restrict"				{ PRINT_DIV("blue", yytext); }
"return"				{ PRINT_DIV("blue", yytext); }
"short"					{ PRINT_DIV("blue", yytext); }
"signed"				{ PRINT_DIV("blue", yytext); }
"sizeof"				{ PRINT_DIV("blue", yytext); }
"static"				{ PRINT_DIV("blue", yytext); }
"struct"				{ PRINT_DIV("blue", yytext); }
"switch"				{ PRINT_DIV("blue", yytext); }
"typedef"				{ PRINT_DIV("blue", yytext); }
"union"					{ PRINT_DIV("blue", yytext); }
"unsigned"				{ PRINT_DIV("blue", yytext); }
"void"					{ PRINT_DIV("blue", yytext); }
"volatile"				{ PRINT_DIV("blue", yytext); }
"while"					{ PRINT_DIV("blue", yytext); }
"_Alignas"              { PRINT_DIV("steelblue", yytext); }
"_Alignof"              { PRINT_DIV("steelblue", yytext); }
"_Atomic"               { PRINT_DIV("steelblue", yytext); }
"_Bool"                 { PRINT_DIV("steelblue", yytext); }
"_Complex"              { PRINT_DIV("steelblue", yytext); }
"_Generic"              { PRINT_DIV("steelblue", yytext); }
"_Imaginary"            { PRINT_DIV("steelblue", yytext); }
"_Noreturn"             { PRINT_DIV("steelblue", yytext); }
"_Static_assert"        { PRINT_DIV("steelblue", yytext); }
"_Thread_local"         { PRINT_DIV("steelblue", yytext); }
"__func__"              { PRINT_DIV("steelblue", yytext); }

{L}{A}*					{ PRINT_DIV("black", yytext); }

{HP}{H}+{IS}?				{ PRINT_DIV("gold", yytext); }
{NZ}{D}*{IS}?				{ PRINT_DIV("gold", yytext); }
"0"{O}*{IS}?				{ PRINT_DIV("gold", yytext); }
{CP}?"'"([^'\\\n]|{ES})+"'"	{ PRINT_DIV("gold", yytext); }

{D}+{E}{FS}?				{ PRINT_DIV("gold", yytext); }
{D}*"."{D}+{E}?{FS}?		{ PRINT_DIV("gold", yytext); }
{D}+"."{E}?{FS}?			{ PRINT_DIV("gold", yytext); }
{HP}{H}+{P}{FS}?			{ PRINT_DIV("gold", yytext); }
{HP}{H}*"."{H}+{P}{FS}?		{ PRINT_DIV("gold", yytext); }
{HP}{H}+"."{P}{FS}?			{ PRINT_DIV("gold", yytext); }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ PRINT_DIV("pink", yytext); }

"..."					{ PRINT_DIV("black", yytext); }
">>="					{ PRINT_DIV("black", yytext); }
"<<="					{ PRINT_DIV("black", yytext); }
"+="					{ PRINT_DIV("black", yytext); }
"-="					{ PRINT_DIV("black", yytext); }
"*="					{ PRINT_DIV("black", yytext); }
"/="					{ PRINT_DIV("black", yytext); }
"%="					{ PRINT_DIV("black", yytext); }
"&="					{ PRINT_DIV("black", yytext); }
"^="					{ PRINT_DIV("black", yytext); }
"|="					{ PRINT_DIV("black", yytext); }
">>"					{ PRINT_DIV("black", yytext); }
"<<"					{ PRINT_DIV("black", yytext); }
"++"					{ PRINT_DIV("black", yytext); }
"--"					{ PRINT_DIV("black", yytext); }
"->"					{ PRINT_DIV("black", yytext); }
"&&"					{ PRINT_DIV("black", yytext); }
"||"					{ PRINT_DIV("black", yytext); }
"<="					{ PRINT_DIV("black", yytext); }
">="					{ PRINT_DIV("black", yytext); }
"=="					{ PRINT_DIV("black", yytext); }
"!="					{ PRINT_DIV("black", yytext); }
";"					    { PRINT_DIV("black", yytext); }
("{"|"<%")				{ PRINT_DIV("brown", yytext); }
("}"|"%>")				{ PRINT_DIV("brown", yytext); }
","					    { PRINT_DIV("black", yytext); }
":"					    { PRINT_DIV("black", yytext); }
"="					    { PRINT_DIV("black", yytext); }
"("					    { PRINT_DIV("brown", yytext); }
")"					    { PRINT_DIV("brown", yytext); }
("["|"<:")				{ PRINT_DIV("brown", yytext); }
("]"|":>")				{ PRINT_DIV("brown", yytext); }
"."					    { PRINT_DIV("black", yytext); }
"&"					    { PRINT_DIV("grey", yytext); }
"!"					    { PRINT_DIV("black", yytext); }
"~"					    { PRINT_DIV("grey", yytext); }
"-"					    { PRINT_DIV("black", yytext); }
"+"					    { PRINT_DIV("black", yytext); }
"*"					    { PRINT_DIV("black", yytext); }
"/"					    { PRINT_DIV("black", yytext); }
"%"					    { PRINT_DIV("black", yytext); }
"<"					    { PRINT_DIV("black", yytext); }
">"					    { PRINT_DIV("black", yytext); }
"^"					    { PRINT_DIV("grey", yytext); }
"|"					    { PRINT_DIV("grey", yytext); }
"?"					    { PRINT_DIV("black", yytext); }

{WS}+					{ PRINT_DIV("white", yytext); }
{NL}+                   { printf("<pre>%s</pre>", yytext); }

.					    { /* discard bad characters */ }

%%

int yywrap(void)        /* called at end of input */
{
    return 1;           /* terminate now */
}

static void comment(void)
{
    int c;

    while ((c = input()) != 0)
        if (c == '*')
        {
            while ((c = input()) == '*')
                ;

            if (c == '/')
                return;

            if (c == 0)
                break;
        }
    yyerror("unterminated comment");
}

static int check_type(void)
{
    switch (sym_type(yytext))
    {
    case TYPEDEF_NAME:                /* previously defined */
        return TYPEDEF_NAME;
    case ENUMERATION_CONSTANT:        /* previously defined */
        return ENUMERATION_CONSTANT;
    default:                          /* includes undefined */
        return IDENTIFIER;
    }
}

int main()
{
    yylex();

    return 0;
}
