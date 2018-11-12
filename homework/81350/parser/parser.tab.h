/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PARSER_TAB_H_INCLUDED
# define YY_YY_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 2 "parser.y" /* yacc.c:1909  */

#include <stdio.h>
#include <string.h>
int yylex();
int yyerror(void* scanner, const char* error);
#undef YYDEBUG
#define YYDEBUG 1

#line 53 "parser.tab.h" /* yacc.c:1909  */

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    NUMBER = 258,
    PLUS = 259,
    MINUS = 260,
    STAR = 261,
    SLASH = 262,
    DOUBLESLASH = 263,
    PERCENT = 264,
    DOUBLESTAR = 265,
    BIGGER = 266,
    LESS = 267,
    BIGGEREQL = 268,
    LESSEQL = 269,
    EQL = 270,
    NOTEQL = 271,
    BOOLAND = 272,
    BOOLOR = 273,
    TILDA = 274,
    HAT = 275,
    SHIFTRIGHT = 276,
    SHIFTLESS = 277,
    ASSIGN = 278,
    PLUSASSIGN = 279,
    MINUSASSIGN = 280,
    STARASSIGN = 281,
    SLASHASSIGN = 282,
    PERCENTASSIGN = 283,
    DOUBLESTARASSIGN = 284,
    DOUBLESLASHASSIGN = 285,
    HATASSIGN = 286,
    BOOLANDASSIGN = 287,
    BOOLORASSIGN = 288,
    SHIFTRIGHTASSIGN = 289,
    SHIFTLEFTASSIGN = 290,
    LPAREN = 291,
    RPAREN = 292,
    SEMICOLON = 293,
    COMMA = 294,
    DOT = 295,
    IDENT = 296
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union name { double Number; char Lexeme[256];}  YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void* yyscanner);

#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */
