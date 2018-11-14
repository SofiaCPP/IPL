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
    ATASSIGN = 291,
    LPAREN = 292,
    RPAREN = 293,
    LSQPAREN = 294,
    RSQPAREN = 295,
    LCURPAREN = 296,
    RCURPAREN = 297,
    SEMICOLON = 298,
    COLON = 299,
    COMMA = 300,
    DOT = 301,
    AT = 302,
    THREEDOTS = 303,
    NAME = 304,
    DEF = 305,
    ASYNC = 306,
    AND = 307,
    OR = 308,
    NOT = 309,
    IN = 310,
    NOTIN = 311,
    IS = 312,
    ISNOT = 313,
    AWAIT = 314,
    NONE = 315,
    TRUE = 316,
    FALSE = 317,
    FOR = 318,
    IF = 319,
    ELIF = 320,
    ELSE = 321,
    EXCEPT = 322,
    CLASS = 323,
    FROM = 324,
    FINALLY = 325,
    LAMBDA = 326,
    YIELD = 327,
    AS = 328,
    ASSERT = 329,
    BREAK = 330,
    CONTINUE = 331,
    DEL = 332,
    GLOBAL = 333,
    IMPORT = 334,
    NONLOCAL = 335,
    PASS = 336,
    RAISE = 337,
    RETURN = 338,
    TRY = 339,
    WHILE = 340,
    WITH = 341,
    INDENT = 342,
    UNINDENT = 343,
    NEWLINE = 344,
    EOFILE = 345,
    STRING = 346,
    DOLAR = 347
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union name {char* lexeme;} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (void);

#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */
