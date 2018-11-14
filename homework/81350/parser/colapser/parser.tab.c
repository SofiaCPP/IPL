/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 2 "parser.y" /* yacc.c:339  */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "stack.h"
int yylex();
int yyerror(const char* error);
FILE *yyin;
#undef YYDEBUG
#define YYDEBUG 1
/* #define YY_DECL int yylex(scanner) void* scanner; */



  char* concatenate3(char* first, char* second, char* third){
    char* result1 = concatenate(first, second);
    char* result2 = concatenate(result1, third);
  }

  void printStyles() {
      puts(
              "    <style>"
              "        .identifier {"
              "            color: black;"
              "        }"
              "        .keyword {"
              "            color: purple;"
              "        }"
              "        .number {"
              "            color: blue;"
              "        }"
              "        .string {"
              "            color: green;"
              "        }"
              "        .operator {"
              "            color: #db184c;"
              "            font-style: bold;"
              "        .constant {"
              "           color: purple;"
              "        }"
              "        .collapsible {"
              "            color: white;"
              "            cursor: pointer;"
              "            padding: 2px;"
              "            width: 2%;"
              "            border: none;"
              "            text-align: center;"
              "            outline: none;"
              "            font-size: 15px;"
              "        }"
              "     </style>"
      );
  }
  void printCollapsableScript() {
          puts(
                  "<script>"
                  "var coll = document.getElementsByClassName(\"collapsible\");\n"
                  "var i;\n"
                  "\n"
                  "for (i = 0; i < coll.length; i++) {\n"
                    "coll[i].addEventListener(\"click\", function() {\n"
                      "this.classList.toggle(\"active\");\n"
                      "var content = this.nextElementSibling;\n"
                      "if (content.style.display === \"inline\") {\n"
                        "content.style.display = \"none\";\n"
                        "this.innerHTML=\"+\""
                      "} else {\n"
                        "content.style.display = \"inline\";\n"
                        "this.innerHTML=\"-\""
                      "}\n"
                    "});\n"
                  "}\n"
                  "for (i = 0; i < coll.length; i++) {\n"
                    "coll[i].click()\n"
                  "}\n"
                  "</script>"
          );
  }
  char* stylize(char* text, char* style) {
      char* result = concatenate("<span class=\"", style);
      result = concatenate(result, "\">");
      result = concatenate(result, text);
      result = concatenate(result, " </span>");
      return result;
  }

  char *str_replace(char *orig, char *rep, char *with) {
      char *result;
      char *ins;
      char *tmp;
      int len_rep;
      int len_with;
      int len_front;
      int count;

      if (!orig || !rep)
          return NULL;
      len_rep = strlen(rep);
      if (len_rep == 0)
          return NULL;
      if (!with)
          with = "";
      len_with = strlen(with);

      // count the number of replacements needed
      ins = orig;
      for (count = 0; tmp = strstr(ins, rep); ++count) {
          ins = tmp + len_rep;
      }

      tmp = result = malloc(strlen(orig) + (len_with - len_rep) * count + 1);

      if (!result)
          return NULL;

      while (count--) {
          ins = strstr(orig, rep);
          len_front = ins - orig;
          tmp = strncpy(tmp, orig, len_front) + len_front;
          tmp = strcpy(tmp, with) + len_with;
          orig += len_front + len_rep; // move to next "end of rep"
      }
      strcpy(tmp, orig);
      return result;
  }

  char* collapsableBlock(char* text) {
          int len = strlen(text);
          if(len>4 && text[len-1] == '>' && text[len-2] == 'r' && text[len-3] == 'b' && text[len-4] == '<'){
            text[len-4] = '\0';
          }
          char* result = concatenate("<button class=\"collapsible\">+</button>", "<div class=\"content\">");
          char* new_text = str_replace(text, "<br>", "<br>&nbsp&nbsp");
          result = concatenate(result, new_text);
          result = concatenate(result, "</div>");
          return result;
  }


#line 206 "parser.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.tab.h".  */
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

/* Copy the second part of user declarations.  */

#line 364 "parser.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  155
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1658

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  93
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  153
/* YYNRULES -- Number of rules.  */
#define YYNRULES  399
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  574

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   347

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   267,   267,   268,   269,   270,   273,   277,   283,   290,
     291,   292,   293,   294,   295,   299,   300,   306,   307,   312,
     313,   314,   315,   319,   320,   324,   325,   326,   330,   331,
     332,   333,   334,   339,   343,   344,   345,   349,   350,   351,
     356,   357,   358,   359,   363,   364,   368,   369,   370,   374,
     375,   376,   377,   382,   386,   387,   388,   392,   393,   397,
     398,   399,   404,   408,   409,   413,   414,   418,   419,   420,
     421,   422,   423,   424,   425,   426,   427,   428,   429,   430,
     431,   432,   436,   437,   438,   439,   440,   441,   446,   450,
     451,   452,   453,   454,   455,   456,   460,   461,   462,   463,
     464,   465,   466,   467,   468,   469,   470,   471,   472,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   493,   494,   495,   496,   501,   502,
     503,   504,   505,   506,   507,   508,   509,   513,   514,   515,
     519,   520,   524,   525,   526,   530,   531,   532,   536,   537,
     538,   539,   543,   544,   545,   546,   550,   551,   555,   556,
     560,   561,   565,   566,   570,   571,   576,   577,   578,   579,
     580,   581,   582,   583,   584,   585,   586,   587,   589,   596,
     602,   608,   613,   618,   622,   623,   625,   630,   637,   644,
     653,   658,   659,   660,   664,   665,   666,   667,   671,   672,
     673,   677,   678,   679,   684,   690,   695,   696,   700,   701,
     705,   706,   713,   714,   717,   718,   724,   725,   726,   727,
     731,   736,   740,   745,   746,   750,   751,   752,   756,   757,
     758,   759,   760,   761,   762,   766,   767,   768,   769,   770,
     771,   772,   773,   774,   775,   776,   777,   778,   779,   780,
     784,   785,   786,   787,   791,   792,   793,   794,   798,   799,
     800,   801,   805,   806,   807,   808,   809,   810,   814,   815,
     816,   817,   818,   819,   823,   824,   825,   826,   827,   828,
     829,   830,   831,   832,   833,   834,   835,   836,   837,   841,
     842,   843,   844,   845,   846,   847,   848,   849,   850,   851,
     852,   853,   854,   855,   856,   857,   858,   859,   860,   861,
     865,   866,   871,   875,   880,   884,   885,   889,   893,   894,
     895,   896,   897,   898,   902,   903,   904,   908,   909,   910,
     911,   912,   916,   917,   918,   922,   927,   928,   929,   930,
     931,   935,   936,   937,   941,   942,   943,   944,   945,   946,
     950,   951,   955,   956,   957,   958,   959,   960,   961,   965,
     966,   967,   968,   969,   970,   971,   975,   976,   977,   978,
     979,   980,   981,   982,   987,   992,   999,  1007,  1008,  1009,
    1013,  1014,  1015,  1019,  1020,  1021,  1025,  1026,  1027,  1028,
    1029,  1034,  1040,  1041,  1042,  1043,  1050,  1051,  1052,  1053
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NUMBER", "PLUS", "MINUS", "STAR",
  "SLASH", "DOUBLESLASH", "PERCENT", "DOUBLESTAR", "BIGGER", "LESS",
  "BIGGEREQL", "LESSEQL", "EQL", "NOTEQL", "BOOLAND", "BOOLOR", "TILDA",
  "HAT", "SHIFTRIGHT", "SHIFTLESS", "ASSIGN", "PLUSASSIGN", "MINUSASSIGN",
  "STARASSIGN", "SLASHASSIGN", "PERCENTASSIGN", "DOUBLESTARASSIGN",
  "DOUBLESLASHASSIGN", "HATASSIGN", "BOOLANDASSIGN", "BOOLORASSIGN",
  "SHIFTRIGHTASSIGN", "SHIFTLEFTASSIGN", "ATASSIGN", "LPAREN", "RPAREN",
  "LSQPAREN", "RSQPAREN", "LCURPAREN", "RCURPAREN", "SEMICOLON", "COLON",
  "COMMA", "DOT", "AT", "THREEDOTS", "NAME", "DEF", "ASYNC", "AND", "OR",
  "NOT", "IN", "NOTIN", "IS", "ISNOT", "AWAIT", "NONE", "TRUE", "FALSE",
  "FOR", "IF", "ELIF", "ELSE", "EXCEPT", "CLASS", "FROM", "FINALLY",
  "LAMBDA", "YIELD", "AS", "ASSERT", "BREAK", "CONTINUE", "DEL", "GLOBAL",
  "IMPORT", "NONLOCAL", "PASS", "RAISE", "RETURN", "TRY", "WHILE", "WITH",
  "INDENT", "UNINDENT", "NEWLINE", "EOFILE", "STRING", "DOLAR", "$accept",
  "input_stmt", "decorator", "decorator_more", "decorated",
  "obj_definition", "async_funcdef", "funcdef", "parameters",
  "typedargslist", "td_one", "td_one_arg", "td_more_arg",
  "td_one_optional", "td_two", "td_two_optional", "td_three", "tfpdef",
  "varargslist", "var_one", "var_one_arg", "var_more_arg",
  "var_one_optional", "var_two", "var_two_optional", "var_three", "vfpdef",
  "stmt", "simple_stmt", "simple_more_stmt", "small_stmt", "expr_stmt",
  "expr_stmt_two", "assign_more", "assign_expr", "aug_expr", "annassign",
  "testlist_star_expr", "testlist_se", "testlist_star_expr_more",
  "augassign", "del_stmt", "pass_stmt", "flow_stmt", "break_stmt",
  "continue_stmt", "return_stmt", "yield_stmt", "raise_stmt",
  "import_stmt", "import_name", "import_from", "import_from_name", "dots",
  "dots_more", "import_from_import", "import_as_name", "dotted_as_name",
  "import_as_names", "import_as_name_more", "dotted_as_names",
  "dotted_as_name_more", "dotted_name", "dotted_name_more", "global_stmt",
  "global_stmt_more", "nonlocal_stmt", "nonlocal_stmt_more", "assert_stmt",
  "compound_stmt", "async_stmt", "if_stmt", "elif_stmt", "else_stmt",
  "elif_stmt_more", "while_stmt", "for_stmt", "try_stmt", "try_stmt_two",
  "finally_stmt", "except_fin_stmt", "except_stmt_more", "except_stmt",
  "except_clause", "with_stmt", "with_item", "with_item_more", "suite",
  "stmt_more", "test", "test_nocond", "lambdadef", "lambdadef_nocond",
  "or_test", "or_test_more", "and_test", "and_test_more", "not_test",
  "comparison", "comp_more", "comp_op", "star_expr", "expr", "expr_more",
  "xor_expr", "xor_expr_more", "and_expr", "and_expr_more", "shift_expr",
  "shift_expr_more", "shift_op", "arith_expr", "arith_expr_more",
  "arith_op", "term", "term_more", "term_op", "factor", "unar_op", "power",
  "atom_expr", "atom", "testlist_comp", "testlist_comp_second",
  "testlist_more_arg", "trailer", "trailer_more", "subscriptlist",
  "subscipt_more", "subscript", "sliceop", "exprlist", "exprlist_more",
  "exprlist_op", "testlist", "testlist_more", "dictorsetmaker",
  "test_star_expr", "dsm_one", "test_star_expr_more", "test_star_expr_2",
  "dsm_two", "test_star_expr_2_more", "classdef", "arglist",
  "arglist_more", "argument", "comp_iter", "sync_comp_for", "comp_for",
  "comp_if", "yield_expr", "yield_arg", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347
};
# endif

#define YYPACT_NINF -424

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-424)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     466,  -424,  -424,  -424,  1566,  -424,   277,  1094,   914,    40,
    -424,  -424,    52,    29,  1540,    36,  -424,  -424,  -424,   316,
    1475,    60,   215,    62,  1210,  1249,  -424,  -424,   316,    73,
      40,    88,  -424,  1475,  1475,    34,  1475,  1475,  -424,  -424,
    -424,   139,  -424,   163,  -424,  -424,  -424,    18,  -424,   171,
      22,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,
    -424,  -424,  -424,  -424,  -424,    91,  -424,  -424,  -424,  -424,
    -424,  -424,  -424,  -424,    87,   158,   181,  -424,  -424,   172,
     205,   231,   230,   170,   252,   135,  1566,  -424,   248,   195,
    -424,  -424,  -424,  -424,   127,   127,   224,   228,  -424,   213,
    1566,  -424,   232,  -424,   226,   174,   197,   225,    23,   247,
    -424,  -424,  -424,  -424,   195,  -424,  -424,   242,   253,   255,
      92,  -424,  -424,   223,  -424,   215,  -424,   167,   254,  1475,
    -424,   261,  -424,   263,  -424,  -424,   286,  1475,   266,  -424,
    -424,  1475,  -424,  -424,   267,   268,  -424,   244,   279,   260,
    -424,   730,   290,   243,   257,  -424,   291,  -424,  -424,  -424,
    -424,  -424,   777,  -424,    21,  1120,  -424,  -424,  -424,  -424,
    -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  1475,
    -424,   317,  -424,  1288,  1327,   298,  -424,  1540,  1540,  -424,
    1540,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,
    -424,  -424,   172,  1566,  1566,   326,  1566,   325,  1566,   330,
    -424,  -424,   170,  1566,  -424,  -424,   252,  1566,  -424,  -424,
    -424,  -424,  -424,   135,  1566,  -424,  1566,   940,  1353,   302,
    -424,   195,  1475,    37,   316,  -424,   307,  -424,  -424,  -424,
    -424,  -424,  -424,  -424,  1475,  -424,  1401,  -424,   309,   311,
    1327,  -424,   313,   314,   312,   320,  1004,  -424,    70,   318,
     195,  1475,   316,   315,   730,  1030,   730,    65,  -424,  -424,
     254,   322,   327,   328,  -424,  1475,    76,   329,  -424,  1475,
    -424,  1475,   334,  -424,   332,   337,    40,   338,   335,   336,
     341,  1475,   300,  -424,   176,   730,   730,  1475,   245,  1566,
    -424,  -424,  -424,   824,  -424,  -424,  -424,  -424,   366,  1120,
    -424,  -424,  -424,  -424,  1327,   331,   158,   181,  1566,  -424,
    -424,  1566,  -424,  1566,  -424,  1566,  1566,  -424,  1566,  -424,
    1566,  -424,  -424,  1475,  1475,  -424,    67,   353,   348,  1427,
     350,   355,   351,  -424,  -424,   354,  -424,   343,  1327,  -424,
     232,  -424,  1401,  -424,  -424,  -424,  1327,  -424,  -424,   352,
     323,   362,   175,   356,  -424,   359,   368,  -424,   364,  -424,
    -424,   381,   730,   367,  -424,   316,   227,   369,   378,  -424,
    -424,   370,   344,  -424,   375,  -424,  -424,    55,  -424,   322,
    -424,  -424,  -424,  -424,    76,  -424,  -424,  -424,  1475,  -424,
     373,  -424,    40,  -424,  -424,   374,  -424,   646,  1475,   380,
    -424,  -424,  -424,   109,  -424,   382,   361,  -424,  -424,   730,
    1475,  -424,  -424,  -424,  1475,  -424,  -424,  1475,  -424,  -424,
    -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  1475,
    -424,  -424,  1184,   383,  1475,   385,  -424,  1427,  -424,  1353,
     386,  -424,  1540,   387,   389,  -424,  -424,  -424,  -424,   346,
     356,   392,   394,   395,  1475,  -424,    85,   397,  -424,  1475,
    -424,   730,  -424,  1475,   400,  -424,  -424,   227,   730,   401,
     408,   376,   370,   402,  -424,  -424,  -424,  -424,  -424,  -424,
    -424,  -424,  -424,  -424,   556,   357,   730,   379,  -424,  -424,
     730,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  1184,  -424,
    -424,   385,  -424,  -424,  1353,    30,  -424,  -424,  -424,  -424,
      89,  -424,   392,  -424,  -424,  -424,  -424,    85,  -424,  -424,
     361,   404,   730,  -424,  -424,  -424,   730,  -424,  -424,  -424,
     370,  -424,  -424,   403,  -424,  -424,  -424,  -424,  -424,  -424,
    1501,  -424,  -424,  -424,  -424,  -424,  -424,  -424,   730,  -424,
    -424,  -424,  -424,    77,    30,  -424,  -424,  -424,  1501,   406,
    -424,  -424,  1475,  -424
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,   298,   281,   282,     0,   283,     0,     0,     0,     0,
     300,   297,     0,     0,     0,     0,   301,   302,   303,     0,
       0,     0,     0,     0,   396,     0,   116,   117,     0,     0,
       0,     0,   110,   121,   118,     0,     0,     0,     2,   299,
     304,     0,     9,     0,   173,   171,     3,     0,    67,    76,
      91,    68,    69,    70,   111,   112,   113,   115,   114,    71,
     124,   125,    72,    73,    74,     5,   174,   166,   167,   168,
     169,   170,    92,   216,   214,   225,   228,   230,    93,   231,
     246,   250,   254,   258,   264,   270,     0,   280,   284,   287,
     172,   120,   245,   290,     0,     0,     0,     0,   293,     0,
       0,   295,   366,   367,     0,   354,   357,   152,     0,     0,
     175,   177,   176,   229,   286,   345,   344,     0,   339,     0,
       0,   131,   132,     0,   133,   130,   128,     0,     0,     0,
      58,     0,    40,     0,    41,    42,    46,     0,   346,   399,
     397,     0,   164,   109,   156,   148,   126,   140,   160,   122,
     119,     0,     0,     0,   206,     1,     0,    10,    11,    14,
      13,    12,     0,    63,     0,     0,    96,    97,    98,   100,
     101,   107,   108,   104,   102,   103,   106,   105,    99,     0,
      75,    79,    77,     0,    90,    89,     4,     0,     0,   223,
       0,   226,   236,   235,   239,   238,   237,   240,   241,   242,
     243,   244,   232,     0,     0,   247,     0,   251,     0,   255,
     263,   262,   259,     0,   268,   269,   265,     0,   274,   276,
     278,   277,   275,   271,     0,   279,     0,     0,     0,     0,
     319,   289,     0,     0,     0,   305,   308,   392,   307,   306,
     292,   291,   294,   359,     0,   296,   353,   352,   363,   360,
     356,   355,   371,   368,     0,   153,     0,     6,     0,     0,
     288,     0,   338,   341,     0,     0,     0,     0,   134,   129,
       0,     0,     0,    57,   219,     0,    51,     0,    44,     0,
     398,   347,   348,   165,     0,   157,     0,   149,     0,     0,
     161,     0,     0,   210,     0,     0,     0,     0,     0,     0,
      15,    64,    65,     0,    62,    80,    83,    82,    86,     0,
      78,    85,    84,    94,    88,     0,   225,   228,     0,   233,
     248,     0,   252,     0,   256,     0,     0,   260,     0,   266,
       0,   272,   285,     0,     0,   315,   384,     0,   378,   328,
     327,     0,   322,   318,   320,   310,   393,     0,     0,   358,
       0,   364,   362,   361,   366,   372,   370,   369,   154,     0,
       0,     0,     0,     0,    17,    38,     0,    19,    31,    20,
      21,    25,     0,     0,   342,   340,   181,     0,     0,   374,
     135,     0,   138,   127,   143,   137,    47,    55,    53,     0,
      56,   220,    49,    50,    51,    43,    45,   350,   349,   158,
       0,   150,     0,   141,   162,     0,   123,     0,   201,     0,
     190,   192,   191,   194,   198,     0,   186,   205,   208,     0,
       0,   207,    61,    66,     0,    81,    95,     0,   224,   227,
     234,   249,   253,   257,   261,   267,   273,   387,   386,     0,
     383,   316,   377,   380,   337,   329,   330,   332,   317,   321,
     324,   309,     0,   312,   314,   365,   373,   155,     7,     0,
       0,     0,     0,    37,     0,    18,    30,    31,    23,     0,
      16,     0,   343,     0,     0,   184,   180,   179,     0,     0,
       0,     0,   142,   145,    48,    54,    52,   351,   159,   151,
     163,   212,    59,    60,     0,   202,     0,   195,   196,   199,
       0,   187,   204,   209,    87,   215,   385,   381,   379,   336,
     331,   333,   334,   325,   323,   390,   311,   313,     8,    26,
      35,    33,     0,    36,    39,    28,    29,    30,    22,    24,
     188,     0,     0,   185,   178,   375,     0,   136,   139,   146,
     144,   211,   213,     0,   193,   197,   200,   382,   335,   326,
       0,   391,   388,   389,    27,    34,    32,   189,     0,   183,
     376,   147,   203,     0,   394,   218,   217,   182,     0,     0,
     395,   221,     0,   222
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -424,  -424,   410,  -424,  -424,  -424,  -424,     2,  -424,  -424,
    -424,  -241,  -322,   -16,   196,   -67,  -242,   -68,  -107,  -424,
    -234,  -108,   180,  -229,    71,  -232,   173,   -35,     3,  -424,
    -142,  -424,  -424,  -424,   152,  -424,  -424,  -153,  -155,  -424,
    -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,  -424,
    -424,  -424,  -424,   340,  -424,  -424,  -423,  -231,    81,  -424,
    -424,  -424,    44,  -424,  -424,  -424,  -424,  -424,  -424,   468,
    -424,  -424,   -11,  -364,  -424,  -424,   460,  -424,  -424,  -365,
    -424,  -424,    61,  -424,   462,  -271,  -424,  -226,  -424,    -6,
     -92,  -424,  -424,  -180,   161,   292,   162,    -1,  -424,  -424,
     276,    15,     5,  -424,  -169,  -424,  -156,  -424,  -171,  -424,
     269,  -181,  -424,   270,  -154,  -424,   259,   -76,  -424,  -424,
    -424,   469,   476,   393,  -424,  -157,   377,  -424,  -424,  -398,
    -391,   -17,  -424,  -205,   -26,  -424,  -424,  -184,  -424,  -424,
    -209,  -424,  -424,   444,   -47,  -424,  -384,   -75,  -424,  -100,
    -424,    -2,  -424
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    41,    42,    43,    44,   158,   159,    45,   259,   366,
     367,   519,   461,   468,   525,   521,   526,   371,   131,   132,
     133,   271,   278,   134,   388,   135,   136,   491,   293,   164,
      47,    48,   180,   181,   305,   310,   182,    49,    50,   185,
     183,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,   123,   124,   125,   383,   384,   145,   385,   483,
     146,   287,   147,   255,    62,   285,    63,   290,    64,   493,
      66,    67,   475,   476,   477,    68,    69,    70,   410,   411,
     412,   413,   414,   415,    71,   153,   298,   294,   494,    72,
     564,    73,   565,    74,   189,    75,   191,    76,    77,   202,
     203,    78,    79,   205,    80,   207,    81,   209,    82,   212,
     213,    83,   216,   217,    84,   223,   224,    85,    86,    87,
      88,    89,    96,   235,   236,   230,   231,   341,   450,   342,
     446,   117,   263,   118,   139,   282,   104,   105,   247,   248,
     106,   251,   252,    90,   337,   443,   338,   551,   237,   238,
     553,    91,   140
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint16 yytable[] =
{
      94,    94,   102,    46,    97,   249,   253,   315,   150,    92,
     225,   143,   306,   113,   119,   110,   370,   368,   138,   142,
     302,    95,    95,   103,   116,   277,   418,   149,   138,   313,
     152,   154,   327,   116,   115,   320,   386,   324,   376,     1,
     379,   355,   386,   115,   393,   160,   467,   392,   498,   497,
     322,   513,   501,   108,   510,   401,   512,   374,   507,   539,
     256,   162,   351,   329,   303,   128,   126,   184,   127,   416,
     417,   380,   128,     6,   344,     7,   362,     8,   151,    12,
     363,   233,   127,   127,    10,    11,   128,   128,   233,   107,
     439,   362,    19,   234,   550,   363,    16,    17,    18,   363,
     234,   109,   381,   344,   130,   243,   129,   163,   364,   120,
     304,   130,   257,   534,   382,    37,   549,   561,   233,   365,
     548,   568,   144,   274,   547,   130,   130,    39,    40,   265,
     234,   280,   545,   346,   365,   283,   266,   148,   365,   155,
     522,   218,   219,   220,   221,   434,   470,   456,   331,   503,
     332,   187,   431,   484,   433,   485,   306,   311,   300,   426,
     484,   423,   393,   307,   389,   392,   557,   432,   455,   269,
     472,   489,   232,   308,   435,   474,   408,   138,   233,   409,
     186,   312,   222,   192,   193,   194,   195,   196,   197,   317,
     234,   210,   211,   502,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   319,   361,
       9,   188,   270,    12,   156,   179,   130,   347,   378,   246,
     460,   336,   340,   204,   365,   233,   345,   198,   199,   200,
     201,    21,   227,   190,   228,   373,   440,   234,   349,   116,
     350,   229,   250,   408,   354,   530,   409,   208,   233,   115,
     336,   206,   535,   242,   436,   138,   214,   215,   226,   336,
     234,   121,   240,   122,   107,   103,   241,   116,   245,   391,
     544,   254,   515,   396,   546,   397,   244,   115,   555,   554,
       1,     2,     3,     4,   258,   406,   554,   296,   297,   419,
     420,   154,   473,   474,   462,   463,     5,   261,   262,   264,
     272,   273,   267,   130,   421,   275,   559,   307,   276,   279,
     560,   281,   284,   286,     6,    93,     7,   288,     8,     1,
       2,     3,     4,   430,   289,    10,    11,   437,   438,   291,
     299,    14,   567,   445,   295,     5,    15,    16,    17,    18,
     309,    12,   453,   314,   321,   323,   350,   325,    23,    24,
     354,   343,   348,     6,   352,     7,   353,     8,   356,   357,
     375,   358,   372,   454,    10,    11,   359,   387,    39,    40,
     566,   103,   270,   390,   394,    15,    16,    17,    18,   398,
     116,   399,   400,   402,   403,   404,   405,   407,   566,   424,
     115,   441,   487,   442,   447,   448,   449,   427,   452,   451,
     459,   457,   495,   464,   469,   365,   465,    39,    40,   466,
     492,   471,   458,   478,   154,   552,   479,   481,   504,   382,
     482,   505,   488,   490,   496,   538,   500,   474,   508,   444,
     543,   514,   516,   506,   517,   518,   336,   520,   509,   460,
     523,   511,   527,   340,   532,   536,   537,   540,   558,   409,
     572,   528,   562,   157,   369,   556,   569,   395,   524,   542,
     486,   425,   480,   529,   552,   268,   533,   531,    65,     1,
       2,     3,     4,   111,   499,   112,   571,   428,   318,   429,
     316,   326,   330,    99,   114,     5,   328,   161,   239,   570,
       0,   260,     0,     0,     0,     0,     0,   492,     0,     0,
       0,     0,   336,     6,     0,     7,     0,     8,   340,     0,
       0,     0,     0,     9,    10,    11,    12,    13,     0,     0,
      14,     0,     0,     0,     0,    15,    16,    17,    18,    19,
      20,     0,     0,     0,    21,    22,     0,    23,    24,     0,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,     0,     0,    38,     0,    39,    40,     1,
       2,     3,     4,     0,     0,     0,   573,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     6,     0,     7,     0,     8,     0,     0,
       0,     0,     0,     9,    10,    11,    12,    13,     0,     0,
      14,     0,     0,     0,     0,    15,    16,    17,    18,    19,
      20,     0,     0,     0,    21,    22,     0,    23,    24,     0,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,     0,   541,     0,     0,    39,    40,     1,
       2,     3,     4,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     6,     0,     7,     0,     8,     0,     0,
       0,     0,     0,     9,    10,    11,    12,    13,     0,     0,
      14,     0,     0,     0,     0,    15,    16,    17,    18,    19,
      20,     0,     0,     0,    21,    22,     0,    23,    24,     0,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,     1,     2,     3,     4,    39,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     6,     0,     7,
       0,     8,     0,     0,     0,     0,     0,     0,    10,    11,
       1,     2,     3,     4,    14,     0,     0,     0,     0,    15,
      16,    17,    18,     0,     0,     0,     5,     0,     0,    22,
       0,    23,    24,     0,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,     6,     0,     7,     0,     8,   292,
       0,    39,    40,     0,     0,    10,    11,     1,     2,     3,
       4,    14,     0,     0,     0,     0,    15,    16,    17,    18,
       0,     0,     0,     5,     0,     0,    22,     0,    23,    24,
       0,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,     6,     0,     7,     0,     8,   301,     0,    39,    40,
       0,     0,    10,    11,     0,     0,     0,     0,    14,     0,
       0,     0,     0,    15,    16,    17,    18,     0,     0,     0,
       0,     0,     0,    22,     0,    23,    24,     0,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,     0,     0,
       0,     0,     0,   422,     0,    39,    40,     1,     2,     3,
       4,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     1,     2,     3,   333,     0,     0,     0,
     334,     6,     0,     7,     0,     8,   101,     0,     0,     5,
       0,     0,    10,    11,     0,     0,     0,     0,    14,     0,
       0,     0,     0,    15,    16,    17,    18,     6,   335,     7,
       0,     8,     0,     0,     0,    23,     0,     0,    10,    11,
       0,     0,     0,     0,    14,     0,     0,     0,     0,    15,
      16,    17,    18,     0,     0,    39,    40,     1,     2,     3,
     333,    23,     0,     0,   334,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     0,     0,     0,     0,     0,     0,
       0,    39,    40,     1,     2,     3,   333,     0,     0,     0,
     334,     6,   360,     7,     0,     8,     0,     0,     0,     5,
       0,     0,    10,    11,     0,     0,     0,     0,    14,     0,
       0,     0,     0,    15,    16,    17,    18,     6,   377,     7,
       0,     8,     0,     0,     0,    23,     0,     0,    10,    11,
       0,     0,     0,     0,    14,     0,     0,     0,     0,    15,
      16,    17,    18,     0,     0,    39,    40,     1,     2,     3,
       4,    23,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     0,     0,     0,     0,     0,     0,
       0,    39,    40,     1,     2,     3,     4,     0,     0,     0,
       0,     6,     0,     7,    98,     8,     0,     0,     0,     5,
       0,     0,    10,    11,     0,     0,     0,     0,    14,     0,
       0,     0,     0,    15,    16,    17,    18,     6,     0,     7,
       0,     8,     0,     0,     0,    23,     0,     0,    10,    11,
       0,     0,     0,     0,    14,     0,     0,     0,     0,    15,
      16,    17,    18,     0,     0,    39,    40,     1,     2,     3,
     333,    23,    24,     0,   334,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     0,     0,     0,     0,     0,     0,
       0,    39,    40,     1,     2,     3,     0,     0,     0,     0,
       0,     6,     0,     7,     0,     8,     0,     0,     0,     5,
       0,     0,    10,    11,     0,     0,     0,     0,    14,     0,
       0,     0,     0,    15,    16,    17,    18,     6,     0,     7,
       0,     8,     1,     2,     3,    23,     0,     0,    10,    11,
       0,     0,     0,     0,    14,     0,     0,     0,     5,    15,
      16,    17,    18,     0,     0,    39,    40,     0,     0,   137,
       0,    23,     0,     0,     0,     0,     6,     0,     7,     0,
       8,     1,     2,     3,   141,     0,     0,    10,    11,     0,
       0,    39,    40,    14,     0,     0,     0,     5,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
      23,     0,     0,     0,     0,     6,     0,     7,     0,     8,
       1,     2,     3,     4,     0,     0,    10,    11,     0,     0,
      39,    40,    14,     0,     0,     0,     5,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     1,     2,     3,    23,
      24,     0,     0,     0,     6,     0,     7,     0,     8,     0,
       0,     0,     5,     0,     0,    10,    11,     0,     0,    39,
      40,    14,     0,     0,     0,     0,    15,    16,    17,    18,
       6,     0,     7,     0,     8,     0,     0,   339,    23,     0,
       0,    10,    11,     0,     1,     2,     3,    14,     0,     0,
       0,   100,    15,    16,    17,    18,     0,     0,    39,    40,
       5,     0,     0,     0,    23,     0,     0,     0,     0,     0,
       1,     2,     3,     0,     0,     0,     0,     0,     6,     0,
       7,     0,     8,     0,    39,    40,     5,     0,     0,    10,
      11,     0,     0,     0,     0,    14,     0,     0,     0,     0,
      15,    16,    17,    18,     6,     0,     7,     0,     8,     0,
       0,   444,    23,     0,     0,    10,    11,     0,     1,     2,
       3,    14,     0,     0,     0,     0,    15,    16,    17,    18,
       0,     0,    39,    40,     5,     0,     0,     0,    23,     0,
       0,     0,     0,     0,     1,     2,     3,     0,     0,     0,
       0,     0,     6,     0,     7,     0,     8,     0,    39,    40,
       5,     0,     0,    10,    11,     0,     0,     0,     0,    14,
       0,     0,     0,     0,    15,    16,    17,    18,     6,     0,
       7,     0,     8,     1,     2,     3,    23,     0,     0,    10,
      11,     0,     0,     0,     0,    14,     0,     0,     0,     5,
      15,    16,    17,    18,     0,     0,    39,    40,     0,     1,
       2,     3,   563,     0,     0,     0,     0,     6,     0,     7,
       0,     8,     0,     0,     0,     5,     0,     0,    10,    11,
       0,     0,    39,    40,    14,     0,     0,     0,     0,    15,
      16,    17,    18,     6,     0,     7,     0,     8,     0,     0,
       0,     0,     0,     0,    10,    11,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    15,    16,    17,    18,     0,
       0,    39,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    39,    40
};

static const yytype_int16 yycheck[] =
{
       6,     7,     8,     0,     6,   105,   106,   187,    34,     4,
      86,    28,   165,    14,    20,    13,   258,   258,    24,    25,
     162,     6,     7,     8,    19,   133,   297,    33,    34,   184,
      36,    37,   213,    28,    19,   204,   270,   208,   264,     3,
     266,   250,   276,    28,   276,    43,   368,   276,   413,   413,
     206,   449,   416,     9,   445,   286,   447,   262,   442,   482,
      37,    43,   246,   217,    43,    10,    22,    45,     6,   295,
     296,     6,    10,    37,   231,    39,     6,    41,    44,    50,
      10,    51,     6,     6,    48,    49,    10,    10,    51,    49,
      23,     6,    63,    63,    64,    10,    60,    61,    62,    10,
      63,    49,    37,   260,    49,   100,    44,    89,    38,    49,
      89,    49,    89,   477,    49,    86,   514,   540,    51,    49,
     511,    44,    49,   129,   508,    49,    49,    91,    92,    37,
      63,   137,   497,   233,    49,   141,    44,    49,    49,     0,
     462,     6,     7,     8,     9,   326,   372,   356,   224,   420,
     226,    64,   321,   387,   325,   387,   309,   183,   156,   314,
     394,   303,   394,   165,   272,   394,   530,   323,   352,   125,
     375,   402,    45,   179,   328,    66,    67,   183,    51,    70,
      89,   183,    47,    11,    12,    13,    14,    15,    16,   190,
      63,    21,    22,   419,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,   203,   256,
      47,    53,    45,    50,    51,    44,    49,   234,   265,    45,
      45,   227,   228,    18,    49,    51,   232,    55,    56,    57,
      58,    68,    37,    52,    39,   261,   336,    63,   244,   234,
     246,    46,    45,    67,   250,   471,    70,    17,    51,   234,
     256,    20,   478,    40,   330,   261,     4,     5,    10,   265,
      63,    46,    38,    48,    49,   250,    38,   262,    42,   275,
     496,    46,   452,   279,   500,   281,    44,   262,   520,   520,
       3,     4,     5,     6,    37,   291,   527,    44,    45,    44,
      45,   297,    65,    66,   362,   363,    19,    55,    45,    44,
     127,   128,    79,    49,   299,    44,   532,   309,    45,    23,
     536,    45,    45,    45,    37,    38,    39,    73,    41,     3,
       4,     5,     6,   318,    45,    48,    49,   333,   334,    69,
      73,    54,   558,   339,    44,    19,    59,    60,    61,    62,
      23,    50,   348,    45,    18,    20,   352,    17,    71,    72,
     356,    49,    45,    37,    45,    39,    45,    41,    45,    45,
      45,    49,    44,   348,    48,    49,    46,    45,    91,    92,
     550,   356,    45,    45,    45,    59,    60,    61,    62,    45,
     375,    49,    45,    45,    49,    49,    45,    87,   568,    23,
     375,    38,   398,    45,    44,    40,    45,    66,    55,    45,
      38,    49,   408,    44,    23,    49,    38,    91,    92,    45,
     407,    44,    89,    44,   420,   515,    38,    73,   424,    49,
      45,   427,    49,    49,    44,    49,    44,    66,    45,    44,
      73,    45,    45,   439,    45,    89,   442,    45,   444,    45,
      45,   447,    45,   449,    44,    44,    38,    45,    44,    70,
      44,   467,    49,    43,   258,   522,   563,   277,   464,   494,
     389,   309,   381,   469,   564,   125,   477,   473,     0,     3,
       4,     5,     6,    13,   413,    13,   568,   316,   202,   317,
     188,   212,   223,     7,    15,    19,   216,    43,    95,   564,
      -1,   114,    -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,
      -1,    -1,   508,    37,    -1,    39,    -1,    41,   514,    -1,
      -1,    -1,    -1,    47,    48,    49,    50,    51,    -1,    -1,
      54,    -1,    -1,    -1,    -1,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    69,    -1,    71,    72,    -1,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    -1,    -1,    89,    -1,    91,    92,     3,
       4,     5,     6,    -1,    -1,    -1,   572,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    -1,    39,    -1,    41,    -1,    -1,
      -1,    -1,    -1,    47,    48,    49,    50,    51,    -1,    -1,
      54,    -1,    -1,    -1,    -1,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    69,    -1,    71,    72,    -1,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    -1,    88,    -1,    -1,    91,    92,     3,
       4,     5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    -1,    39,    -1,    41,    -1,    -1,
      -1,    -1,    -1,    47,    48,    49,    50,    51,    -1,    -1,
      54,    -1,    -1,    -1,    -1,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    69,    -1,    71,    72,    -1,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     3,     4,     5,     6,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    -1,    39,
      -1,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
       3,     4,     5,     6,    54,    -1,    -1,    -1,    -1,    59,
      60,    61,    62,    -1,    -1,    -1,    19,    -1,    -1,    69,
      -1,    71,    72,    -1,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    37,    -1,    39,    -1,    41,    89,
      -1,    91,    92,    -1,    -1,    48,    49,     3,     4,     5,
       6,    54,    -1,    -1,    -1,    -1,    59,    60,    61,    62,
      -1,    -1,    -1,    19,    -1,    -1,    69,    -1,    71,    72,
      -1,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    37,    -1,    39,    -1,    41,    89,    -1,    91,    92,
      -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    -1,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    71,    72,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    92,     3,     4,     5,
       6,    -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,    -1,    -1,    -1,
      10,    37,    -1,    39,    -1,    41,    42,    -1,    -1,    19,
      -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    -1,    59,    60,    61,    62,    37,    38,    39,
      -1,    41,    -1,    -1,    -1,    71,    -1,    -1,    48,    49,
      -1,    -1,    -1,    -1,    54,    -1,    -1,    -1,    -1,    59,
      60,    61,    62,    -1,    -1,    91,    92,     3,     4,     5,
       6,    71,    -1,    -1,    10,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,     3,     4,     5,     6,    -1,    -1,    -1,
      10,    37,    38,    39,    -1,    41,    -1,    -1,    -1,    19,
      -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    -1,    59,    60,    61,    62,    37,    38,    39,
      -1,    41,    -1,    -1,    -1,    71,    -1,    -1,    48,    49,
      -1,    -1,    -1,    -1,    54,    -1,    -1,    -1,    -1,    59,
      60,    61,    62,    -1,    -1,    91,    92,     3,     4,     5,
       6,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,     3,     4,     5,     6,    -1,    -1,    -1,
      -1,    37,    -1,    39,    40,    41,    -1,    -1,    -1,    19,
      -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    -1,    59,    60,    61,    62,    37,    -1,    39,
      -1,    41,    -1,    -1,    -1,    71,    -1,    -1,    48,    49,
      -1,    -1,    -1,    -1,    54,    -1,    -1,    -1,    -1,    59,
      60,    61,    62,    -1,    -1,    91,    92,     3,     4,     5,
       6,    71,    72,    -1,    10,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,     3,     4,     5,    -1,    -1,    -1,    -1,
      -1,    37,    -1,    39,    -1,    41,    -1,    -1,    -1,    19,
      -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    -1,    59,    60,    61,    62,    37,    -1,    39,
      -1,    41,     3,     4,     5,    71,    -1,    -1,    48,    49,
      -1,    -1,    -1,    -1,    54,    -1,    -1,    -1,    19,    59,
      60,    61,    62,    -1,    -1,    91,    92,    -1,    -1,    69,
      -1,    71,    -1,    -1,    -1,    -1,    37,    -1,    39,    -1,
      41,     3,     4,     5,    45,    -1,    -1,    48,    49,    -1,
      -1,    91,    92,    54,    -1,    -1,    -1,    19,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    37,    -1,    39,    -1,    41,
       3,     4,     5,     6,    -1,    -1,    48,    49,    -1,    -1,
      91,    92,    54,    -1,    -1,    -1,    19,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    71,
      72,    -1,    -1,    -1,    37,    -1,    39,    -1,    41,    -1,
      -1,    -1,    19,    -1,    -1,    48,    49,    -1,    -1,    91,
      92,    54,    -1,    -1,    -1,    -1,    59,    60,    61,    62,
      37,    -1,    39,    -1,    41,    -1,    -1,    44,    71,    -1,
      -1,    48,    49,    -1,     3,     4,     5,    54,    -1,    -1,
      -1,    10,    59,    60,    61,    62,    -1,    -1,    91,    92,
      19,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,    -1,    -1,    -1,    -1,    37,    -1,
      39,    -1,    41,    -1,    91,    92,    19,    -1,    -1,    48,
      49,    -1,    -1,    -1,    -1,    54,    -1,    -1,    -1,    -1,
      59,    60,    61,    62,    37,    -1,    39,    -1,    41,    -1,
      -1,    44,    71,    -1,    -1,    48,    49,    -1,     3,     4,
       5,    54,    -1,    -1,    -1,    -1,    59,    60,    61,    62,
      -1,    -1,    91,    92,    19,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,    -1,    -1,
      -1,    -1,    37,    -1,    39,    -1,    41,    -1,    91,    92,
      19,    -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    54,
      -1,    -1,    -1,    -1,    59,    60,    61,    62,    37,    -1,
      39,    -1,    41,     3,     4,     5,    71,    -1,    -1,    48,
      49,    -1,    -1,    -1,    -1,    54,    -1,    -1,    -1,    19,
      59,    60,    61,    62,    -1,    -1,    91,    92,    -1,     3,
       4,     5,    71,    -1,    -1,    -1,    -1,    37,    -1,    39,
      -1,    41,    -1,    -1,    -1,    19,    -1,    -1,    48,    49,
      -1,    -1,    91,    92,    54,    -1,    -1,    -1,    -1,    59,
      60,    61,    62,    37,    -1,    39,    -1,    41,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    59,    60,    61,    62,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,    19,    37,    39,    41,    47,
      48,    49,    50,    51,    54,    59,    60,    61,    62,    63,
      64,    68,    69,    71,    72,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    89,    91,
      92,    94,    95,    96,    97,   100,   121,   123,   124,   130,
     131,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   157,   159,   161,   162,   163,   164,   168,   169,
     170,   177,   182,   184,   186,   188,   190,   191,   194,   195,
     197,   199,   201,   204,   207,   210,   211,   212,   213,   214,
     236,   244,   195,    38,   182,   194,   215,   244,    40,   215,
      10,    42,   182,   194,   229,   230,   233,    49,   155,    49,
     100,   169,   177,   190,   214,   194,   195,   224,   226,   182,
      49,    46,    48,   145,   146,   147,   155,     6,    10,    44,
      49,   111,   112,   113,   116,   118,   119,    69,   182,   227,
     245,    45,   182,   224,    49,   150,   153,   155,    49,   182,
     227,    44,   182,   178,   182,     0,    51,    95,    98,    99,
     100,   236,    43,    89,   122,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    44,
     125,   126,   129,   133,    45,   132,    89,    64,    53,   187,
      52,   189,    11,    12,    13,    14,    15,    16,    55,    56,
      57,    58,   192,   193,    18,   196,    20,   198,    17,   200,
      21,    22,   202,   203,     4,     5,   205,   206,     6,     7,
       8,     9,    47,   208,   209,   210,    10,    37,    39,    46,
     218,   219,    45,    51,    63,   216,   217,   241,   242,   216,
      38,    38,    40,   195,    44,    42,    45,   231,   232,   242,
      45,   234,   235,   242,    46,   156,    37,    89,    37,   101,
     219,    55,    45,   225,    44,    37,    44,    79,   146,   155,
      45,   114,   119,   119,   182,    44,    45,   114,   115,    23,
     182,    45,   228,   182,    45,   158,    45,   154,    73,    45,
     160,    69,    89,   121,   180,    44,    44,    45,   179,    73,
     100,    89,   123,    43,    89,   127,   130,   244,   182,    23,
     128,   227,   244,   131,    45,   186,   188,   190,   193,   195,
     197,    18,   199,    20,   201,    17,   203,   204,   206,   207,
     209,   210,   210,     6,    10,    38,   182,   237,   239,    44,
     182,   220,   222,    49,   218,   182,   242,   224,    45,   182,
     182,   230,    45,    45,   182,   233,    45,    45,    49,    46,
      38,   237,     6,    10,    38,    49,   102,   103,   104,   107,
     109,   110,    44,   227,   226,    45,   180,    38,   237,   180,
       6,    37,    49,   148,   149,   151,   113,    45,   117,   114,
      45,   182,   116,   118,    45,   115,   182,   182,    45,    49,
      45,   150,    45,    49,    49,    45,   182,    87,    67,    70,
     171,   172,   173,   174,   175,   176,   180,   180,   178,    44,
      45,   195,    89,   123,    23,   127,   131,    66,   187,   189,
     195,   197,   199,   201,   204,   207,   210,   182,   182,    23,
     242,    38,    45,   238,    44,   182,   223,    44,    40,    45,
     221,    45,    55,   182,   194,   230,   233,    49,    89,    38,
      45,   105,   110,   110,    44,    38,    45,   105,   106,    23,
     180,    44,   226,    65,    66,   165,   166,   167,    44,    38,
     151,    73,    45,   152,   113,   118,   117,   182,    49,   150,
      49,   120,   121,   162,   181,   182,    44,   166,   172,   175,
      44,   166,   180,   178,   182,   182,   182,   239,    45,   182,
     223,   182,   223,   222,    45,   186,    45,    45,    89,   104,
      45,   108,   105,    45,   182,   107,   109,    45,   106,   182,
     180,   182,    44,   165,   166,   180,    44,    38,    49,   149,
      45,    88,   120,    73,   180,   172,   180,   239,   223,   222,
      64,   240,   242,   243,   104,   109,   108,   166,    44,   180,
     180,   149,    49,    71,   183,   185,   186,   180,    44,   111,
     240,   183,    44,   182
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    93,    94,    94,    94,    94,    95,    95,    95,    96,
      96,    97,    98,    98,    98,    99,   100,   101,   101,   102,
     102,   102,   103,   103,   104,   104,   105,   105,   106,   106,
     106,   106,   107,   107,   108,   108,   109,   109,   110,   110,
     111,   111,   111,   112,   112,   113,   113,   114,   114,   115,
     115,   115,   116,   116,   117,   117,   118,   118,   119,   120,
     120,   121,   121,   121,   121,   122,   122,   123,   123,   123,
     123,   123,   123,   123,   123,   124,   124,   125,   125,   125,
     126,   126,   127,   127,   128,   128,   129,   129,   130,   130,
     130,   130,   131,   131,   132,   132,   133,   133,   133,   133,
     133,   133,   133,   133,   133,   133,   133,   133,   133,   134,
     135,   136,   136,   136,   136,   136,   137,   138,   139,   139,
     140,   141,   141,   141,   142,   142,   143,   144,   145,   145,
     145,   146,   146,   147,   147,   148,   148,   148,   149,   149,
     150,   150,   151,   151,   151,   151,   152,   152,   153,   153,
     154,   154,   155,   155,   156,   156,   157,   157,   158,   158,
     159,   159,   160,   160,   161,   161,   162,   162,   162,   162,
     162,   162,   162,   162,   162,   163,   163,   163,   164,   164,
     164,   164,   165,   166,   167,   167,   168,   168,   169,   169,
     170,   171,   171,   172,   173,   173,   173,   173,   174,   174,
     175,   176,   176,   176,   177,   177,   178,   178,   179,   179,
     180,   180,   181,   181,   182,   182,   182,   183,   183,   184,
     184,   185,   185,   186,   187,   187,   188,   189,   189,   190,
     190,   191,   191,   192,   192,   193,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   194,   195,   195,   196,   196,
     197,   197,   198,   198,   199,   199,   200,   200,   201,   201,
     202,   202,   203,   203,   204,   204,   205,   205,   206,   206,
     207,   207,   208,   208,   209,   209,   209,   209,   209,   210,
     210,   211,   211,   211,   212,   212,   213,   213,   213,   213,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   214,
     214,   214,   214,   214,   214,   215,   215,   216,   216,   217,
     217,   217,   217,   217,   217,   218,   218,   218,   218,   219,
     219,   220,   220,   220,   220,   221,   221,   222,   222,   222,
     222,   222,   222,   222,   222,   222,   223,   223,   224,   224,
     224,   224,   225,   225,   226,   226,   227,   227,   227,   227,
     228,   228,   229,   229,   229,   229,   229,   229,   230,   230,
     231,   231,   231,   231,   232,   232,   233,   233,   234,   234,
     234,   234,   235,   235,   236,   236,   236,   237,   237,   237,
     237,   238,   238,   239,   239,   239,   239,   239,   240,   240,
     241,   241,   242,   242,   243,   243,   244,   244,   245,   245
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     3,     5,     6,     1,
       2,     2,     1,     1,     1,     2,     5,     2,     3,     1,
       1,     1,     3,     2,     3,     1,     2,     3,     2,     2,
       1,     0,     4,     3,     2,     1,     3,     2,     1,     3,
       1,     1,     1,     3,     2,     3,     1,     2,     3,     2,
       2,     1,     4,     3,     2,     1,     3,     2,     1,     1,
       1,     4,     3,     2,     3,     2,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     2,     1,
       2,     3,     1,     1,     1,     1,     2,     4,     3,     2,
       2,     1,     1,     1,     2,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     2,     4,     1,     1,     2,     4,     1,     2,
       1,     1,     1,     1,     2,     1,     3,     1,     1,     3,
       1,     3,     2,     1,     3,     2,     2,     3,     1,     2,
       2,     3,     1,     2,     2,     3,     2,     3,     2,     3,
       2,     3,     2,     3,     2,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     6,     5,
       5,     4,     4,     3,     1,     2,     4,     5,     6,     7,
       4,     1,     1,     3,     1,     2,     2,     3,     1,     2,
       3,     1,     2,     4,     5,     4,     1,     3,     2,     3,
       1,     4,     1,     2,     1,     5,     1,     1,     1,     3,
       4,     3,     4,     2,     3,     0,     2,     3,     0,     2,
       1,     1,     2,     2,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     2,     2,     3,
       1,     2,     2,     3,     1,     2,     2,     3,     1,     2,
       2,     3,     1,     1,     1,     2,     2,     3,     1,     1,
       1,     2,     2,     3,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     3,     2,     1,     3,     2,
       2,     3,     3,     2,     3,     2,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     1,     3,
       2,     4,     3,     4,     3,     2,     3,     3,     2,     1,
       2,     2,     1,     3,     2,     2,     3,     1,     1,     2,
       2,     3,     2,     3,     3,     4,     2,     1,     2,     1,
       3,     2,     2,     3,     1,     1,     1,     2,     2,     3,
       2,     3,     2,     2,     1,     2,     2,     1,     3,     2,
       1,     2,     2,     1,     2,     3,     1,     1,     1,     2,
       2,     1,     2,     3,     4,     6,     7,     2,     1,     3,
       2,     2,     3,     2,     1,     3,     2,     2,     1,     1,
       4,     5,     1,     2,     2,     3,     1,     2,     2,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 267 "parser.y" /* yacc.c:1646  */
    { printf("\n");}
#line 2236 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 268 "parser.y" /* yacc.c:1646  */
    { printf("%s", (yyval.lexeme));}
#line 2242 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 269 "parser.y" /* yacc.c:1646  */
    { printf("%s\n", (yyval.lexeme));}
#line 2248 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 270 "parser.y" /* yacc.c:1646  */
    { printf("%s", (yyval.lexeme));}
#line 2254 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 273 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "operator"), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), "\n");
                        }
#line 2263 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 277 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate(stylize((yyvsp[-4].lexeme), "operator"), (yyvsp[-3].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), "\n");
                        }
#line 2274 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 283 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate(stylize((yyvsp[-5].lexeme), "operator"), (yyvsp[-4].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-3].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), "\n");
         }
#line 2286 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 290 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2292 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 291 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2298 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 292 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2304 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 293 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2310 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 294 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2316 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 295 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2322 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 299 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 2328 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 300 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.lexeme) = concatenate(stylize((yyvsp[-4].lexeme), "keyword"), stylize((yyvsp[-3].lexeme), "identifier"));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                }
#line 2339 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 306 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2345 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 307 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                }
#line 2354 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 312 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2360 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 313 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2366 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 314 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2372 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 315 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 2381 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 319 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2387 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 320 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 2396 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 324 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2402 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 325 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2408 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 326 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                   (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                 }
#line 2417 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 330 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2423 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 331 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2429 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 332 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2435 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 333 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = ""; }
#line 2441 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 334 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "operator"), (yyvsp[-2].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                        }
#line 2451 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 339 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "operator"), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                        }
#line 2460 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 343 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2466 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 344 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2472 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 345 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "operator"), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 2481 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 349 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 2487 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 350 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2493 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 351 "parser.y" /* yacc.c:1646  */
    {
              (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "identifier"), (yyvsp[-1].lexeme));
              (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
            }
#line 2502 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 356 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2508 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 357 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2514 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 358 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2520 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 359 "parser.y" /* yacc.c:1646  */
    {
                      (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                      (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                    }
#line 2529 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 363 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2535 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 364 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 2544 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 368 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2550 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 369 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2556 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 370 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                    (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                  }
#line 2565 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 374 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2571 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 375 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2577 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 376 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2583 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 377 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "operator"), (yyvsp[-2].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 2593 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 382 "parser.y" /* yacc.c:1646  */
    {
               (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "operator"), (yyvsp[-1].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
             }
#line 2602 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 386 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2608 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 387 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2614 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 388 "parser.y" /* yacc.c:1646  */
    {
          (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "operator"), (yyvsp[-1].lexeme));
          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
        }
#line 2623 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 392 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 2629 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 393 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "identifier"); }
#line 2635 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 397 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2641 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 398 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2647 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 399 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.lexeme) = concatenate((yyvsp[-3].lexeme), (yyvsp[-2].lexeme));
                    (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                    (yyval.lexeme) = concatenate((yyval.lexeme), "<br>");
                  }
#line 2657 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 404 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                    (yyval.lexeme) = concatenate((yyval.lexeme), "<br>");
                  }
#line 2666 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 408 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), "<br>");}
#line 2672 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 409 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                    (yyval.lexeme) = concatenate((yyval.lexeme), "<br>");
                  }
#line 2681 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 413 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2687 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 414 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 2696 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 418 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2702 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 419 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2708 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 420 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2714 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 421 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2720 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 422 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2726 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 423 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2732 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 424 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2738 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 425 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2744 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 426 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2750 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 427 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2756 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 428 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2762 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 429 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2768 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 430 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2774 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 431 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 2780 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 432 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                   (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                 }
#line 2789 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 436 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2795 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 437 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2801 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 438 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2807 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 439 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2813 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 440 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2819 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 441 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.lexeme) = concatenate((yyvsp[-3].lexeme), (yyvsp[-2].lexeme));
                 (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                 (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
               }
#line 2829 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 446 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 2838 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 450 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2844 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 451 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2850 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 452 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2856 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 453 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2862 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 454 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2868 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 455 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 2874 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 456 "parser.y" /* yacc.c:1646  */
    {
                               (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                             }
#line 2883 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 460 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2889 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 461 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2895 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 462 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2901 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 463 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2907 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 464 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2913 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 465 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2919 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 466 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2925 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 467 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2931 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 468 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2937 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 105:
#line 469 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2943 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 470 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2949 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 471 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2955 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 472 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 2961 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 474 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 2967 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 110:
#line 475 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "keyword");}
#line 2973 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 111:
#line 476 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2979 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 477 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2985 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 113:
#line 478 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2991 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 114:
#line 479 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 2997 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 115:
#line 480 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3003 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 481 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "keyword");}
#line 3009 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 482 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "keyword");}
#line 3015 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 483 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "keyword");}
#line 3021 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 484 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3027 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 485 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3033 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 486 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "keyword");}
#line 3039 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 487 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3045 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 488 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-1].lexeme), "keyword"));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                }
#line 3055 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 493 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3061 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 125:
#line 494 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3067 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 126:
#line 495 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3073 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 496 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-1].lexeme), "keyword"));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 3083 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 501 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3089 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 502 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3095 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 503 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3101 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 131:
#line 504 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3107 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 132:
#line 505 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3113 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 133:
#line 506 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3119 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 507 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3125 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 508 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3131 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 136:
#line 509 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                        }
#line 3140 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 137:
#line 513 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3146 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 138:
#line 514 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "identifier");}
#line 3152 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 139:
#line 515 "parser.y" /* yacc.c:1646  */
    {
                      (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "identifier"), stylize((yyvsp[-1].lexeme), "operator"));
                      (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[0].lexeme), "identifier"));
                    }
#line 3161 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 519 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3167 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 520 "parser.y" /* yacc.c:1646  */
    {
                      (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                      (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[0].lexeme), "identifier"));
                    }
#line 3176 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 524 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3182 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 525 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3188 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 144:
#line 526 "parser.y" /* yacc.c:1646  */
    {
                       (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                       (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                     }
#line 3197 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 530 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3203 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 531 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3209 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 532 "parser.y" /* yacc.c:1646  */
    {
                           (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                           (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                         }
#line 3218 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 148:
#line 536 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3224 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 149:
#line 537 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3230 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 150:
#line 538 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3236 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 151:
#line 539 "parser.y" /* yacc.c:1646  */
    {
                           (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                           (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                         }
#line 3245 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 152:
#line 543 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "identifier");}
#line 3251 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 153:
#line 544 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "identifier"), (yyvsp[0].lexeme));}
#line 3257 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 154:
#line 545 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), stylize((yyvsp[0].lexeme), "identifier"));}
#line 3263 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 155:
#line 546 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[0].lexeme), "identifier"));
                      }
#line 3272 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 156:
#line 550 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), stylize((yyvsp[0].lexeme), "identifier"));}
#line 3278 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 157:
#line 551 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), stylize((yyvsp[-1].lexeme), "identifier"));
                   (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                 }
#line 3287 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 158:
#line 555 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), stylize((yyvsp[0].lexeme), "identifier"));}
#line 3293 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 159:
#line 556 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[0].lexeme), "identifier"));
                      }
#line 3302 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 160:
#line 560 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), stylize((yyvsp[0].lexeme), "identifier"));}
#line 3308 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 161:
#line 561 "parser.y" /* yacc.c:1646  */
    {
                     (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), stylize((yyvsp[-1].lexeme), "identifier"));
                     (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                   }
#line 3317 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 162:
#line 565 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), stylize((yyvsp[0].lexeme), "identifier"));}
#line 3323 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 163:
#line 566 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[0].lexeme), "identifier"));;
                        }
#line 3332 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 164:
#line 570 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3338 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 165:
#line 571 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), (yyvsp[-1].lexeme));
                   (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                 }
#line 3347 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 166:
#line 576 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3353 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 167:
#line 577 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3359 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 168:
#line 578 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3365 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 579 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3371 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 170:
#line 580 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3377 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 171:
#line 581 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3383 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 172:
#line 582 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3389 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 173:
#line 583 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3395 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 174:
#line 584 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3401 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 175:
#line 585 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3407 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 176:
#line 586 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3413 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 177:
#line 587 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3419 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 178:
#line 589 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.lexeme) = concatenate(stylize((yyvsp[-5].lexeme), "keyword"), (yyvsp[-4].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-3].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                }
#line 3431 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 179:
#line 596 "parser.y" /* yacc.c:1646  */
    {
               (yyval.lexeme) = concatenate(stylize((yyvsp[-4].lexeme), "keyword"), (yyvsp[-3].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
             }
#line 3442 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 180:
#line 602 "parser.y" /* yacc.c:1646  */
    {
               (yyval.lexeme) = concatenate(stylize((yyvsp[-4].lexeme), "keyword"), (yyvsp[-3].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
             }
#line 3453 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 181:
#line 608 "parser.y" /* yacc.c:1646  */
    {
               (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
             }
#line 3463 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 182:
#line 613 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 3473 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 183:
#line 618 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 3482 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 184:
#line 622 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3488 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 185:
#line 623 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3494 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 186:
#line 625 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 3504 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 187:
#line 630 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.lexeme) = concatenate(stylize((yyvsp[-4].lexeme), "keyword"), (yyvsp[-3].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                }
#line 3515 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 637 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate(stylize((yyvsp[-5].lexeme), "keyword"), (yyvsp[-4].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-3].lexeme), "operator"));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 3527 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 644 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-6].lexeme), "keyword"), (yyvsp[-5].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-4].lexeme), "operator"));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-3].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 3540 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 190:
#line 653 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 3550 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 658 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3556 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 659 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3562 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 660 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 3571 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 194:
#line 664 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3577 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 665 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3583 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 666 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3589 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 667 "parser.y" /* yacc.c:1646  */
    {
                       (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                       (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                     }
#line 3598 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 198:
#line 671 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3604 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 672 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3610 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 200:
#line 673 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 3619 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 677 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "keyword"); }
#line 3625 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 678 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 3631 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 679 "parser.y" /* yacc.c:1646  */
    {
                     (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
                     (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                     (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[0].lexeme), "identifier"));
                   }
#line 3641 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 204:
#line 684 "parser.y" /* yacc.c:1646  */
    {
        (yyval.lexeme) = concatenate(stylize((yyvsp[-4].lexeme), "keyword"), (yyvsp[-3].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
      }
#line 3652 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 690 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
                 (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                 (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
               }
#line 3662 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 695 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3668 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 696 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                 (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
               }
#line 3677 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 700 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3683 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 701 "parser.y" /* yacc.c:1646  */
    {
                              (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                              (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                            }
#line 3692 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 705 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3698 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 706 "parser.y" /* yacc.c:1646  */
    {
                     (yyval.lexeme) = concatenate("<br>", (yyvsp[-2].lexeme));
                     (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                     (yyval.lexeme) = concatenate((yyval.lexeme), "");
                  /*   $$ = concatenate($$, $4); */
                     (yyval.lexeme) = collapsableBlock((yyval.lexeme));
                   }
#line 3710 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 713 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3716 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 714 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3722 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 717 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3728 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 718 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.lexeme) = concatenate((yyvsp[-4].lexeme), stylize((yyvsp[-3].lexeme), "keyword"));
                    (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                    (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-1].lexeme), "keyword"));
                    (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                  }
#line 3739 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 724 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3745 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 725 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3751 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 726 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3757 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 727 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 3766 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 731 "parser.y" /* yacc.c:1646  */
    {
                         (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
                         (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                         (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                       }
#line 3776 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 736 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 3785 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 740 "parser.y" /* yacc.c:1646  */
    {
                                (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
                                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                              }
#line 3795 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 745 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3801 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 746 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "operator"), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 3810 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 225:
#line 750 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = ""; }
#line 3816 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 751 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3822 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 227:
#line 752 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "operator"), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 3831 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 228:
#line 756 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = ""; }
#line 3837 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 757 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 3843 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 758 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3849 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 231:
#line 759 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3855 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 760 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3861 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 761 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3867 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 234:
#line 762 "parser.y" /* yacc.c:1646  */
    {
                         (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                         (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                       }
#line 3876 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 235:
#line 766 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3882 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 236:
#line 767 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3888 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 237:
#line 768 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3894 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 238:
#line 769 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3900 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 239:
#line 770 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3906 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 240:
#line 771 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3912 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 772 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3918 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 773 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3924 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 774 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3930 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 244:
#line 775 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 3936 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 245:
#line 776 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 3942 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 246:
#line 777 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3948 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 247:
#line 778 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3954 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 248:
#line 779 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 3960 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 780 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                    (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                  }
#line 3969 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 784 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 3975 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 785 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 3981 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 786 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 3987 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 787 "parser.y" /* yacc.c:1646  */
    {
                             (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                             (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                           }
#line 3996 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 254:
#line 791 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4002 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 255:
#line 792 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4008 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 256:
#line 793 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 4014 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 257:
#line 794 "parser.y" /* yacc.c:1646  */
    {
                             (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                             (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                           }
#line 4023 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 258:
#line 798 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4029 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 259:
#line 799 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4035 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 800 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4041 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 801 "parser.y" /* yacc.c:1646  */
    {
                               (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                             }
#line 4050 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 805 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4056 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 263:
#line 806 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4062 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 264:
#line 807 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4068 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 265:
#line 808 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4074 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 266:
#line 809 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4080 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 267:
#line 810 "parser.y" /* yacc.c:1646  */
    {
                               (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                               (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                             }
#line 4089 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 268:
#line 814 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4095 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 269:
#line 815 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4101 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 270:
#line 816 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4107 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 271:
#line 817 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4113 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 272:
#line 818 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4119 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 273:
#line 819 "parser.y" /* yacc.c:1646  */
    {
                         (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                         (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                       }
#line 4128 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 823 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4134 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 824 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4140 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 825 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4146 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 277:
#line 826 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4152 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 278:
#line 827 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4158 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 279:
#line 828 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme)); }
#line 4164 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 280:
#line 829 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4170 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 281:
#line 830 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4176 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 282:
#line 831 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4182 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 283:
#line 832 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4188 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 284:
#line 833 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4194 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 285:
#line 834 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate3((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 4200 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 286:
#line 835 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 4206 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 287:
#line 836 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4212 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 288:
#line 837 "parser.y" /* yacc.c:1646  */
    {
                         (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), (yyvsp[-1].lexeme));
                         (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                       }
#line 4221 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 289:
#line 841 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4227 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 290:
#line 842 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4233 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 291:
#line 843 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate3((yyvsp[-2].lexeme), (yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4239 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 292:
#line 844 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate3((yyvsp[-2].lexeme), (yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4245 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 293:
#line 845 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4251 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 294:
#line 846 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate3((yyvsp[-2].lexeme), (yyvsp[-1].lexeme), (yyvsp[0].lexeme)); }
#line 4257 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 295:
#line 847 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4263 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 296:
#line 848 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate3((yyvsp[-2].lexeme), (yyvsp[-1].lexeme), (yyvsp[0].lexeme)); }
#line 4269 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 297:
#line 849 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "identifier");}
#line 4275 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 298:
#line 850 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "number");}
#line 4281 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 299:
#line 851 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "string");}
#line 4287 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 300:
#line 852 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "operator");}
#line 4293 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 301:
#line 853 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "constant");}
#line 4299 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 302:
#line 854 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "constant");}
#line 4305 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 303:
#line 855 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "constant");}
#line 4311 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 856 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = "";}
#line 4317 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 857 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4323 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 306:
#line 858 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4329 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 307:
#line 859 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4335 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 308:
#line 860 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4341 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 309:
#line 861 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 4350 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 310:
#line 865 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4356 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 311:
#line 866 "parser.y" /* yacc.c:1646  */
    {
                                  (yyval.lexeme) = concatenate((yyvsp[-3].lexeme), (yyvsp[-2].lexeme));
                                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                                }
#line 4366 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 312:
#line 871 "parser.y" /* yacc.c:1646  */
    {
                                  (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                                }
#line 4375 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 313:
#line 875 "parser.y" /* yacc.c:1646  */
    {
                                  (yyval.lexeme) = concatenate((yyvsp[-3].lexeme), (yyvsp[-2].lexeme));
                                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                                }
#line 4385 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 314:
#line 880 "parser.y" /* yacc.c:1646  */
    {
                                  (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                                  (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                                }
#line 4394 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 315:
#line 884 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4400 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 316:
#line 885 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 4409 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 317:
#line 889 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 4418 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 318:
#line 893 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), stylize((yyvsp[0].lexeme), "identifier"));}
#line 4424 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 319:
#line 894 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4430 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 895 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4436 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 896 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4442 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 897 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4448 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 323:
#line 898 "parser.y" /* yacc.c:1646  */
    {
                              (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                              (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                            }
#line 4457 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 324:
#line 902 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4463 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 325:
#line 903 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4469 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 326:
#line 904 "parser.y" /* yacc.c:1646  */
    {
                             (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                             (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                           }
#line 4478 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 327:
#line 908 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4484 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 328:
#line 909 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4490 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 329:
#line 910 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4496 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 911 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4502 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 331:
#line 912 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                        }
#line 4511 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 332:
#line 916 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4517 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 917 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));}
#line 4523 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 918 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                        }
#line 4532 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 335:
#line 922 "parser.y" /* yacc.c:1646  */
    {
                          (yyval.lexeme) = concatenate((yyvsp[-3].lexeme), (yyvsp[-2].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                          (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                        }
#line 4542 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 336:
#line 927 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4548 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 337:
#line 928 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4554 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 338:
#line 929 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4560 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 339:
#line 930 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4566 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 340:
#line 931 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 4575 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 341:
#line 935 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4581 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 342:
#line 936 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4587 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 937 "parser.y" /* yacc.c:1646  */
    {
                             (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                             (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                           }
#line 4596 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 941 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4602 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 942 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4608 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 943 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4614 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 944 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4620 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 348:
#line 945 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4626 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 946 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 4635 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 350:
#line 950 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4641 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 351:
#line 951 "parser.y" /* yacc.c:1646  */
    {
                             (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                             (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                           }
#line 4650 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 352:
#line 955 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4656 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 956 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4662 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 957 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4668 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 958 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4674 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 959 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4680 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 960 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4686 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 961 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 4695 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 965 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 4701 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 360:
#line 966 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4707 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 361:
#line 967 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4713 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 968 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4719 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 363:
#line 969 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4725 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 364:
#line 970 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4731 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 971 "parser.y" /* yacc.c:1646  */
    {
                                   (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                                   (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                                 }
#line 4740 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 975 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4746 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 367:
#line 976 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4752 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 368:
#line 977 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4758 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 978 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4764 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 979 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4770 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 371:
#line 980 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4776 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 981 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4782 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 982 "parser.y" /* yacc.c:1646  */
    {
                                     (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                                     (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                                   }
#line 4791 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 987 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), stylize((yyvsp[-2].lexeme), "identifier"));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 4801 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 992 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate(stylize((yyvsp[-5].lexeme), "keyword"), stylize((yyvsp[-4].lexeme), "identifier"));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-3].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 4813 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 999 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate(stylize((yyvsp[-6].lexeme), "keyword"), stylize((yyvsp[-5].lexeme), "identifier"));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-4].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-3].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-2].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 4826 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 1007 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4832 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 1008 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4838 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 1009 "parser.y" /* yacc.c:1646  */
    {
                       (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                       (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                     }
#line 4847 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 1013 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4853 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 1014 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4859 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 1015 "parser.y" /* yacc.c:1646  */
    {
                            (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), (yyvsp[-1].lexeme));
                            (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                          }
#line 4868 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 383:
#line 1019 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate((yyvsp[-1].lexeme), (yyvsp[0].lexeme));}
#line 4874 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 1020 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4880 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 385:
#line 1021 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.lexeme) = concatenate((yyvsp[-2].lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                        (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                      }
#line 4889 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 386:
#line 1025 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 4895 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 387:
#line 1026 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "operator"), (yyvsp[0].lexeme));}
#line 4901 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 1027 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4907 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 1028 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4913 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 1029 "parser.y" /* yacc.c:1646  */
    {
                (yyval.lexeme) = concatenate(stylize((yyvsp[-3].lexeme), "keyword"), (yyvsp[-2].lexeme));
                (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-1].lexeme), "operator"));
                (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
              }
#line 4923 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 391:
#line 1034 "parser.y" /* yacc.c:1646  */
    {
                             (yyval.lexeme) = concatenate(stylize((yyvsp[-4].lexeme), "keyword"), (yyvsp[-3].lexeme));
                             (yyval.lexeme) = concatenate((yyval.lexeme), stylize((yyvsp[-2].lexeme), "operator"));
                             (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[-1].lexeme));
                             (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                           }
#line 4934 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 392:
#line 1040 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4940 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 393:
#line 1041 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 4946 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 394:
#line 1042 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 4952 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 395:
#line 1043 "parser.y" /* yacc.c:1646  */
    {
                       (yyval.lexeme) = concatenate(stylize((yyvsp[-2].lexeme), "keyword"), (yyvsp[-1].lexeme));
                       (yyval.lexeme) = concatenate((yyval.lexeme), (yyvsp[0].lexeme));
                     }
#line 4961 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 396:
#line 1050 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = stylize((yyvsp[0].lexeme), "keyword");}
#line 4967 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 397:
#line 1051 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 4973 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 398:
#line 1052 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = concatenate(stylize((yyvsp[-1].lexeme), "keyword"), (yyvsp[0].lexeme));}
#line 4979 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 399:
#line 1053 "parser.y" /* yacc.c:1646  */
    { (yyval.lexeme) = (yyvsp[0].lexeme); }
#line 4985 "parser.tab.c" /* yacc.c:1646  */
    break;


#line 4989 "parser.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[1] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 1055 "parser.y" /* yacc.c:1906  */

int yyerror(const char* error)
{
    extern int current_line;
    fprintf(stderr, "input:%d: error: %s\n", current_line, error);
    return 0;
}

void printHtmlHeader(){
       puts(
                   "<head>"
                   "    <title>python-parse.html</title>"
       );
       printStyles();
       puts(
        "</head>"
       );
}
void printBody() {
    puts(
      "<body bgcolor=\"#e2cdb7\">"
      "    <pre class=\"code\">"
      );
      yyparse();
      puts("</pre></body>");
}
void printHTML() {
    puts(
      "<!doctype>"
      "<html>"
    );
    printHtmlHeader();
    printBody();
    printCollapsableScript();
    puts("</html>");
}


int main(int argc, const char* argv[])
{
  #if YYDEBUG
    yydebug = 1;
  #endif
    //printHTML();
    FILE *myfile = fopen("input.file", "r");
if (!myfile) {
  printf("I can't open input.file!\n");
  return -1;
}
yyin = myfile;
    printHTML();
    return 0;
}
