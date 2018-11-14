
%{
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

%}

%start input_stmt
/* %define api.pure full */
%define api.value.type {union name {char* lexeme;}}
%locations

%type<lexeme> NUMBER PLUS MINUS STAR SLASH DOUBLESLASH PERCENT DOUBLESTAR BIGGER
%type<lexeme> LESS BIGGEREQL LESSEQL EQL NOTEQL
%type<lexeme> BOOLAND BOOLOR TILDA HAT SHIFTRIGHT SHIFTLESS
%type<lexeme> ASSIGN PLUSASSIGN MINUSASSIGN STARASSIGN SLASHASSIGN PERCENTASSIGN
%type<lexeme> DOUBLESTARASSIGN DOUBLESLASHASSIGN HATASSIGN BOOLANDASSIGN
%type<lexeme> BOOLORASSIGN SHIFTRIGHTASSIGN SHIFTLEFTASSIGN ATASSIGN
%type<lexeme> LPAREN RPAREN LSQPAREN RSQPAREN LCURPAREN RCURPAREN SEMICOLON
%type<lexeme> COLON COMMA DOT AT THREEDOTS NAME
%type<lexeme> DEF ASYNC AND OR NOT IN NOTIN IS ISNOT AWAIT NONE TRUE FALSE FOR
%type<lexeme> IF ELIF ELSE EXCEPT CLASS FROM FINALLY LAMBDA YIELD AS ASSERT
%type<lexeme> BREAK CONTINUE DEL GLOBAL IMPORT NONLOCAL PASS RAISE RETURN TRY WHILE WITH
%type<lexeme> INDENT UNINDENT NEWLINE STRING
%type<lexeme>  input_stmt decorator decorator_more decorated obj_definition async_funcdef funcdef parameters typedargslist td_one td_one_arg td_more_arg td_one_optional td_two td_two_optional td_three tfpdef varargslist var_one var_one_arg var_more_arg var_one_optional var_two var_two_optional var_three vfpdef stmt simple_stmt simple_more_stmt small_stmt expr_stmt expr_stmt_two assign_more assign_expr aug_expr annassign testlist_star_expr testlist_se testlist_star_expr_more augassign del_stmt pass_stmt flow_stmt break_stmt continue_stmt return_stmt yield_stmt raise_stmt import_stmt import_name import_from import_from_name dots dots_more import_from_import import_as_name dotted_as_name import_as_names import_as_name_more dotted_as_names dotted_as_name_more dotted_name dotted_name_more global_stmt global_stmt_more nonlocal_stmt nonlocal_stmt_more assert_stmt compound_stmt async_stmt if_stmt elif_stmt else_stmt elif_stmt_more while_stmt for_stmt try_stmt try_stmt_two finally_stmt except_fin_stmt except_stmt_more except_stmt except_clause with_stmt with_item with_item_more suite stmt_more test test_nocond lambdadef lambdadef_nocond or_test or_test_more and_test and_test_more not_test comparison comp_more comp_op star_expr expr expr_more xor_expr xor_expr_more and_expr and_expr_more shift_expr shift_expr_more shift_op arith_expr arith_expr_more arith_op term term_more term_op factor unar_op power atom_expr atom testlist_comp testlist_comp_second testlist_more_arg trailer trailer_more subscriptlist subscipt_more subscript sliceop exprlist exprlist_more exprlist_op testlist testlist_more dictorsetmaker test_star_expr dsm_one test_star_expr_more test_star_expr_2 dsm_two test_star_expr_2_more classdef arglist arglist_more argument comp_iter sync_comp_for comp_for comp_if yield_expr yield_arg



%token NUMBER
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token DOUBLESLASH
%token PERCENT
%token DOUBLESTAR
%token BIGGER
%token LESS
%token BIGGEREQL
%token LESSEQL
%token EQL
%token NOTEQL

%token BOOLAND
%token BOOLOR
%token TILDA
%token HAT
%token SHIFTRIGHT
%token SHIFTLESS

%token ASSIGN
%token PLUSASSIGN
%token MINUSASSIGN
%token STARASSIGN
%token SLASHASSIGN
%token PERCENTASSIGN
%token DOUBLESTARASSIGN
%token DOUBLESLASHASSIGN
%token HATASSIGN
%token BOOLANDASSIGN
%token BOOLORASSIGN
%token SHIFTRIGHTASSIGN
%token SHIFTLEFTASSIGN
%token ATASSIGN

%token LPAREN
%token RPAREN
%token LSQPAREN
%token RSQPAREN
%token LCURPAREN
%token RCURPAREN
%token SEMICOLON
%token COLON
%token COMMA
%token DOT
%token AT
%token THREEDOTS

%token NAME

%token DEF
%token ASYNC
%token AND
%token OR
%token NOT
%token IN
%token NOTIN
%token IS
%token ISNOT
%token AWAIT
%token NONE
%token TRUE
%token FALSE
%token FOR
%token IF
%token ELIF
%token ELSE
%token EXCEPT
%token CLASS
%token FROM
%token FINALLY
%token LAMBDA
%token YIELD
%token AS
%token ASSERT
%token BREAK
%token CONTINUE
%token DEL
%token GLOBAL
%token IMPORT
%token NONLOCAL
%token PASS
%token RAISE
%token RETURN
%token TRY
%token WHILE
%token WITH

%token INDENT
%token UNINDENT
%token NEWLINE
%token EOFILE
%token STRING
%token DOLAR

/*
Define both - precedence and associativity */
%left PLUS MINUS
%left STAR SLASH

%%

input_stmt: NEWLINE                     { printf("\n");}
          | simple_stmt                 { printf("%s", $$);}
          | compound_stmt NEWLINE       { printf("%s\n", $$);}
          | compound_stmt               { printf("%s", $$);}


decorator: AT dotted_name NEWLINE {
                          $$ = concatenate(stylize($1, "operator"), $2);
                          $$ = concatenate($$, "\n");
                        }
         | AT dotted_name LPAREN RPAREN NEWLINE {
                          $$ = concatenate(stylize($1, "operator"), $2);
                          $$ = concatenate($$, $3);
                          $$ = concatenate($$, $4);
                          $$ = concatenate($$, "\n");
                        }
         | AT dotted_name LPAREN arglist RPAREN NEWLINE {
                        $$ = concatenate(stylize($1, "operator"), $2);
                        $$ = concatenate($$, $3);
                        $$ = concatenate($$, $4);
                        $$ = concatenate($$, $5);
                        $$ = concatenate($$, "\n");
         }
decorator_more: decorator { $$ = $1; }
              | decorator_more decorator { $$ = concatenate($1, $2);}
decorated: decorator_more obj_definition { $$ = concatenate($1, $2);}
obj_definition: classdef { $$ = $1; }
              | funcdef { $$ = $1; }
              | async_funcdef { $$ = $1; }

/* gotovo do tuk */

async_funcdef: ASYNC funcdef { $$ = concatenate(stylize($1, "keyword"), $2);}
funcdef: DEF NAME parameters COLON suite {
                  $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));
                  $$ = concatenate($$, $3);
                  $$ = concatenate($$, $4);
                  $$ = concatenate($$, $5);
                }
parameters: LPAREN RPAREN { $$ = concatenate($1, $2);}
          | LPAREN typedargslist RPAREN {
                  $$ = concatenate($1, $2);
                  $$ = concatenate($$, $3);
                }

typedargslist: td_one { $$ = $1; }
             | td_two { $$ = $1; }
             | td_three { $$ = $1; }
td_one: td_one_arg td_more_arg td_one_optional {
        $$ = concatenate($1, $2);
        $$ = concatenate($$, $3);
      }
      | td_one_arg td_one_optional { $$ = concatenate($1, $2);}
td_one_arg: tfpdef ASSIGN test {
        $$ = concatenate($1, stylize($2, "operator"));
        $$ = concatenate($$, $3);
      }
          | tfpdef { $$ = $1; }
td_more_arg: COMMA td_one_arg { $$ = concatenate($1, $2);}
           | td_more_arg COMMA td_one_arg {
                   $$ = concatenate($1, $2);
                   $$ = concatenate($$, $3);
                 }
td_one_optional: COMMA td_two { $$ = concatenate($1, $2);}
               | COMMA td_three { $$ = concatenate($1, $2);}
               | COMMA { $$ = $1; }
               | { $$ = ""; }
td_two: STAR tfpdef td_more_arg td_two_optional {
                          $$ = concatenate(stylize($1, "operator"), $2);
                          $$ = concatenate($$, $3);
                          $$ = concatenate($$, $4);
                        }
                  | STAR td_more_arg td_two_optional {
                          $$ = concatenate(stylize($1, "operator"), $2);
                          $$ = concatenate($$, $3);
                        }
td_two_optional: COMMA td_three { $$ = concatenate($1, $2);}
               | COMMA { $$ = $1; }
td_three: DOUBLESTAR tfpdef COMMA {
                        $$ = concatenate(stylize($1, "operator"), $2);
                        $$ = concatenate($$, $3);
                      }
        | DOUBLESTAR tfpdef { $$ = concatenate(stylize($1, "operator"), $2);}
tfpdef: NAME { $$ = $1; }
      | NAME COLON test {
              $$ = concatenate(stylize($1, "identifier"), $2);
              $$ = concatenate($$, $3);
            }

varargslist: var_one { $$ = $1; }
           | var_two { $$ = $1; }
           | var_three { $$ = $1; }
var_one: var_one_arg var_more_arg var_one_optional {
                      $$ = concatenate($1, $2);
                      $$ = concatenate($$, $3);
                    }
       | var_one_arg var_one_optional { $$ = concatenate($1, $2);}
var_one_arg: vfpdef ASSIGN test {
        $$ = concatenate($1, stylize($2, "operator"));
        $$ = concatenate($$, $3);
      }
           | vfpdef { $$ = $1; }
var_more_arg: COMMA var_one_arg { $$ = concatenate($1, $2);}
            | var_more_arg COMMA var_one_arg {
                    $$ = concatenate($1, $2);
                    $$ = concatenate($$, $3);
                  }
var_one_optional: COMMA var_two { $$ = concatenate($1, $2);}
                | COMMA var_three { $$ = concatenate($1, $2);}
                | COMMA { $$ = $1; }
var_two: STAR vfpdef var_more_arg var_two_optional {
                $$ = concatenate(stylize($1, "operator"), $2);
                $$ = concatenate($$, $3);
                $$ = concatenate($$, $4);
              }
       | STAR var_more_arg var_two_optional {
               $$ = concatenate(stylize($1, "operator"), $2);
               $$ = concatenate($$, $3);
             }
var_two_optional: COMMA var_three { $$ = concatenate($1, $2);}
                | COMMA { $$ = $1; }
var_three: DOUBLESTAR vfpdef COMMA {
          $$ = concatenate(stylize($1, "operator"), $2);
          $$ = concatenate($$, $3);
        }
         | DOUBLESTAR vfpdef { $$ = concatenate(stylize($1, "operator"), $2);}
vfpdef: NAME { $$ = stylize($1, "identifier"); }

/* do tuk e gotovo */

stmt: simple_stmt { $$ = $1; }
    | compound_stmt { $$ = $1; }
simple_stmt: small_stmt simple_more_stmt SEMICOLON NEWLINE {
                    $$ = concatenate($1, $2);
                    $$ = concatenate($$, $3);
                    $$ = concatenate($$, "<br>");
                  }
            | small_stmt simple_more_stmt NEWLINE {
                    $$ = concatenate($1, $2);
                    $$ = concatenate($$, "<br>");
                  }
            | small_stmt NEWLINE { $$ = concatenate($1, "<br>");}
            | small_stmt SEMICOLON NEWLINE {
                    $$ = concatenate($1, $2);
                    $$ = concatenate($$, "<br>");
                  }
simple_more_stmt: SEMICOLON small_stmt { $$ = concatenate($1, $2);}
                | simple_more_stmt SEMICOLON small_stmt {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, $3);
                      }
small_stmt: expr_stmt { $$ = $1; }
          | del_stmt { $$ = $1; }
          | pass_stmt { $$ = $1; }
          | flow_stmt { $$ = $1; }
          | import_stmt { $$ = $1; }
          | global_stmt { $$ = $1; }
          | nonlocal_stmt { $$ = $1; }
          | assert_stmt { $$ = $1; }
expr_stmt: testlist_star_expr expr_stmt_two { $$ = concatenate($1, $2);}
         | testlist_star_expr { $$ = $1; }
expr_stmt_two: annassign { $$ = $1; }
             | augassign aug_expr { $$ = concatenate($1, $2);}
             | assign_more { $$ = $1; }
assign_more: ASSIGN assign_expr { $$ = concatenate(stylize($1, "operator"), $2);}
           | assign_more ASSIGN assign_expr {
                   $$ = concatenate($1, stylize($2, "operator"));
                   $$ = concatenate($$, $3);
                 }
assign_expr: yield_expr { $$ = $1; }
           | testlist_star_expr { $$ = $1; }
aug_expr: yield_expr { $$ = $1; }
        | testlist { $$ = $1; }
annassign: COLON test { $$ = concatenate($1, $2);}
         | COLON test ASSIGN test {
                 $$ = concatenate($1, $2);
                 $$ = concatenate($$, stylize($3, "operator"));
                 $$ = concatenate($$, $4);
               }
testlist_star_expr: testlist_se testlist_star_expr_more COMMA {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, $3);
                      }
                  | testlist_se testlist_star_expr_more { $$ = concatenate($1, $2);}
                  | testlist_se COMMA { $$ = concatenate($1, $2);}
                  | testlist_se { $$ = $1; }
testlist_se: test { $$ = $1; }
           | star_expr { $$ = $1; }
testlist_star_expr_more: COMMA testlist_se { $$ = concatenate($1, $2);}
                       | testlist_star_expr_more COMMA testlist_se {
                               $$ = concatenate($1, $2);
                               $$ = concatenate($$, $3);
                             }
augassign: PLUSASSIGN { $$ = stylize($1, "operator");}
         | MINUSASSIGN { $$ = stylize($1, "operator");}
         | STARASSIGN { $$ = stylize($1, "operator");}
         | ATASSIGN { $$ = stylize($1, "operator");}
         | SLASHASSIGN { $$ = stylize($1, "operator");}
         | PERCENTASSIGN { $$ = stylize($1, "operator");}
         | BOOLANDASSIGN { $$ = stylize($1, "operator");}
         | BOOLORASSIGN { $$ = stylize($1, "operator");}
         | HATASSIGN { $$ = stylize($1, "operator");}
         | SHIFTLEFTASSIGN { $$ = stylize($1, "operator");}
         | SHIFTRIGHTASSIGN { $$ = stylize($1, "operator");}
         | DOUBLESTARASSIGN { $$ = stylize($1, "operator");}
         | DOUBLESLASHASSIGN { $$ = stylize($1, "operator");}

del_stmt: DEL exprlist { $$ = concatenate(stylize($1, "keyword"), $2);}
pass_stmt: PASS { $$ = stylize($1, "keyword");}
flow_stmt: break_stmt { $$ = $1; }
         | continue_stmt { $$ = $1; }
         | return_stmt { $$ = $1; }
         | raise_stmt { $$ = $1; }
         | yield_stmt { $$ = $1; }
break_stmt: BREAK { $$ = stylize($1, "keyword");}
continue_stmt: CONTINUE { $$ = stylize($1, "keyword");}
return_stmt: RETURN { $$ = stylize($1, "keyword");}
           | RETURN testlist { $$ = concatenate(stylize($1, "keyword"), $2);}
yield_stmt: yield_expr { $$ = $1; }
raise_stmt: RAISE { $$ = stylize($1, "keyword");}
          | RAISE test { $$ = concatenate(stylize($1, "keyword"), $2);}
          | RAISE test FROM test {
                  $$ = concatenate(stylize($1, "keyword"), $2);
                  $$ = concatenate($$, stylize($3, "keyword"));
                  $$ = concatenate($$, $4);
                }
import_stmt: import_name { $$ = $1; }
           | import_from { $$ = $1; }
import_name: IMPORT dotted_as_names { $$ = concatenate(stylize($1, "keyword"), $2);}
import_from: FROM import_from_name IMPORT import_from_import {
                $$ = concatenate(stylize($1, "keyword"), $2);
                $$ = concatenate($$, stylize($3, "keyword"));
                $$ = concatenate($$, $4);
              }
import_from_name: dotted_name { $$ = $1; }
                | dots_more dotted_name { $$ = concatenate($1, $2);}
                | dots_more { $$ = $1; }
dots: DOT { $$ = $1; }
    | THREEDOTS { $$ = $1; }
dots_more: dots { $$ = $1; }
         | dots_more dots { $$ = concatenate($1, $2);}
import_from_import: STAR { $$ = stylize($1, "operator");}
                  | LPAREN import_as_names RPAREN {
                          $$ = concatenate($1, $2);
                          $$ = concatenate($$, $3);
                        }
                  | import_as_names { $$ = $1; }
import_as_name: NAME { $$ = stylize($1, "identifier");}
              | NAME AS NAME {
                      $$ = concatenate(stylize($1, "identifier"), stylize($2, "operator"));
                      $$ = concatenate($$, stylize($3, "identifier"));
                    }
dotted_as_name: dotted_name { $$ = $1; }
              | dotted_name AS NAME {
                      $$ = concatenate($1, stylize($2, "operator"));
                      $$ = concatenate($$, stylize($3, "identifier"));
                    }
import_as_names: import_as_name COMMA { $$ = concatenate($1, $2);}
               | import_as_name { $$ = $1; }
               | import_as_name import_as_name_more COMMA {
                       $$ = concatenate($1, $2);
                       $$ = concatenate($$, $3);
                     }
               | import_as_name import_as_name_more { $$ = concatenate($1, $2);}
import_as_name_more: COMMA import_as_name { $$ = concatenate($1, $2);}
                   | import_as_name_more COMMA import_as_name {
                           $$ = concatenate($1, $2);
                           $$ = concatenate($$, $3);
                         }
dotted_as_names: dotted_as_name { $$ = $1; }
               | dotted_as_name dotted_as_name_more { $$ = concatenate($1, $2);}
dotted_as_name_more: COMMA dotted_as_name { $$ = concatenate($1, $2);}
                   | dotted_as_name_more COMMA dotted_as_name {
                           $$ = concatenate($1, $2);
                           $$ = concatenate($$, $3);
                         }
dotted_name: NAME { $$ = stylize($1, "identifier");}
           | NAME dotted_name_more { $$ = concatenate(stylize($1, "identifier"), $2);}
dotted_name_more: DOT NAME { $$ = concatenate($1, stylize($2, "identifier"));}
                | dotted_name_more DOT NAME {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, stylize($3, "identifier"));
                      }
global_stmt: GLOBAL NAME { $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));}
           | GLOBAL NAME global_stmt_more {
                   $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));
                   $$ = concatenate($$, $3);
                 }
global_stmt_more: COMMA NAME { $$ = concatenate($1, stylize($2, "identifier"));}
                | global_stmt_more COMMA NAME {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, stylize($3, "identifier"));
                      }
nonlocal_stmt: NONLOCAL NAME { $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));}
             | NONLOCAL NAME nonlocal_stmt_more {
                     $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));
                     $$ = concatenate($$, $3);
                   }
nonlocal_stmt_more: COMMA NAME { $$ = concatenate($1, stylize($2, "identifier"));}
                  | nonlocal_stmt_more COMMA NAME {
                          $$ = concatenate($1, $2);
                          $$ = concatenate($$, stylize($3, "identifier"));;
                        }
assert_stmt: ASSERT test { $$ = concatenate(stylize($1, "keyword"), $2);}
           | ASSERT COMMA test {
                   $$ = concatenate(stylize($1, "keyword"), $2);
                   $$ = concatenate($$, $3);
                 }

compound_stmt: if_stmt { $$ = $1; }
             | while_stmt { $$ = $1; }
             | for_stmt { $$ = $1; }
             | try_stmt { $$ = $1; }
             | with_stmt { $$ = $1; }
             | funcdef { $$ = $1; }
             | classdef { $$ = $1; }
             | decorated { $$ = $1; }
             | async_stmt { $$ = $1; }
async_stmt: ASYNC funcdef { $$ = concatenate(stylize($1, "keyword"), $2);}
          | ASYNC with_stmt { $$ = concatenate(stylize($1, "keyword"), $2);}
          | ASYNC for_stmt { $$ = concatenate(stylize($1, "keyword"), $2);}

if_stmt: IF test COLON suite elif_stmt_more else_stmt {
                  $$ = concatenate(stylize($1, "keyword"), $2);
                  $$ = concatenate($$, $3);
                  $$ = concatenate($$, $4);
                  $$ = concatenate($$, $5);
                  $$ = concatenate($$, $6);
                }
       | IF test COLON suite elif_stmt_more {
               $$ = concatenate(stylize($1, "keyword"), $2);
               $$ = concatenate($$, $3);
               $$ = concatenate($$, $4);
               $$ = concatenate($$, $5);
             }
       | IF test COLON suite else_stmt {
               $$ = concatenate(stylize($1, "keyword"), $2);
               $$ = concatenate($$, $3);
               $$ = concatenate($$, $4);
               $$ = concatenate($$, $5);
             }
       | IF test COLON suite {
               $$ = concatenate(stylize($1, "keyword"), $2);
               $$ = concatenate($$, $3);
               $$ = concatenate($$, $4);
             }
elif_stmt: ELIF test COLON suite {
        $$ = $$ = concatenate(stylize($1, "keyword"), $2);
        $$ = concatenate($$, $3);
        $$ = concatenate($$, $4);
      }
else_stmt: ELSE COLON suite {
        $$ = $$ = concatenate(stylize($1, "keyword"), $2);
        $$ = concatenate($$, $3);
      }
elif_stmt_more: elif_stmt { $$ = $1; }
              | elif_stmt_more elif_stmt { $$ = concatenate($1, $2);}

while_stmt: WHILE test COLON suite {
        $$ = concatenate(stylize($1, "keyword"), $2);
        $$ = concatenate($$, $3);
        $$ = concatenate($$, $4);
      }
          | WHILE test COLON suite else_stmt {
                  $$ = concatenate(stylize($1, "keyword"), $2);
                  $$ = concatenate($$, $3);
                  $$ = concatenate($$, $4);
                  $$ = concatenate($$, $5);
                }

for_stmt: FOR exprlist IN testlist COLON suite {
        $$ = concatenate(stylize($1, "keyword"), $2);
        $$ = concatenate($$, stylize($3, "operator"));
        $$ = concatenate($$, $4);
        $$ = concatenate($$, $5);
        $$ = concatenate($$, $6);
      }
        | FOR exprlist IN testlist COLON suite else_stmt {
                $$ = concatenate(stylize($1, "keyword"), $2);
                $$ = concatenate($$, stylize($3, "operator"));
                $$ = concatenate($$, $4);
                $$ = concatenate($$, $5);
                $$ = concatenate($$, $6);
                $$ = concatenate($$, $7);
              }

try_stmt: TRY COLON suite try_stmt_two {
        $$ = concatenate(stylize($1, "keyword"), $2);
        $$ = concatenate($$, $3);
        $$ = concatenate($$, $4);
      }
try_stmt_two: except_fin_stmt { $$ = $1; }
            | finally_stmt { $$ = $1; }
finally_stmt: FINALLY COLON suite {
        $$ = concatenate(stylize($1, "keyword"), $2);
        $$ = concatenate($$, $3);
      }
except_fin_stmt: except_stmt_more { $$ = $1; }
               | except_stmt_more else_stmt { $$ = concatenate($1, $2);}
               | except_stmt_more finally_stmt { $$ = concatenate($1, $2);}
               | except_stmt_more else_stmt finally_stmt {
                       $$ = concatenate($1, $2);
                       $$ = concatenate($$, $3);
                     }
except_stmt_more: except_stmt  { $$ = $1; }
                | except_stmt_more except_stmt { $$ = concatenate($1, $2);}
except_stmt: except_clause COLON suite  {
                $$ = concatenate($1, $2);
                $$ = concatenate($$, $3);
              }
except_clause: EXCEPT { $$ = stylize($1, "keyword"); }
             | EXCEPT test { $$ = concatenate(stylize($1, "keyword"), $2);}
             | EXCEPT test AS NAME  {
                     $$ = concatenate(stylize($1, "keyword"), $2);
                     $$ = concatenate($$, stylize($3, "operator"));
                     $$ = concatenate($$, stylize($4, "identifier"));
                   }
with_stmt: WITH with_item with_item_more COLON suite {
        $$ = concatenate(stylize($1, "keyword"), $2);
        $$ = concatenate($$, $3);
        $$ = concatenate($$, $4);
        $$ = concatenate($$, $5);
      }
         | WITH with_item COLON suite {
                 $$ = concatenate(stylize($1, "keyword"), $2);
                 $$ = concatenate($$, $3);
                 $$ = concatenate($$, $4);
               }
with_item: test { $$ = $1; }
         | test AS expr {
                 $$ = concatenate($1, stylize($2, "operator"));
                 $$ = concatenate($$, $3);
               }
with_item_more: COMMA with_item { $$ = concatenate($1, $2);}
              | with_item_more COMMA with_item {
                              $$ = concatenate($1, $2);
                              $$ = concatenate($$, $3);
                            }
suite: simple_stmt { $$ = $1; }
     | NEWLINE INDENT stmt_more UNINDENT {
                     $$ = concatenate("<br>", $2);
                     $$ = concatenate($2, $3);
                     $$ = concatenate($$, "");
                  /*   $$ = concatenate($$, $4); */
                     $$ = collapsableBlock($$);
                   }
stmt_more: stmt { $$ = $1; }
         | stmt_more stmt { $$ = concatenate($1, $2);}


test: or_test { $$ = $1; }
    | or_test IF or_test ELSE test {
                    $$ = concatenate($1, stylize($2, "keyword"));
                    $$ = concatenate($$, $3);
                    $$ = concatenate($$, stylize($4, "keyword"));
                    $$ = concatenate($$, $5);
                  }
    | lambdadef { $$ = $1; }
test_nocond: or_test  { $$ = $1; }
           | lambdadef_nocond { $$ = $1; }
lambdadef: LAMBDA COLON test {
                $$ = concatenate(stylize($1, "keyword"), $2);
                $$ = concatenate($$, $3);
              }
         | LAMBDA varargslist COLON test {
                         $$ = concatenate(stylize($1, "keyword"), $2);
                         $$ = concatenate($$, $3);
                         $$ = concatenate($$, $4);
                       }
lambdadef_nocond: LAMBDA COLON test_nocond {
                $$ = concatenate(stylize($1, "keyword"), $2);
                $$ = concatenate($$, $3);
              }
                | LAMBDA varargslist COLON test {
                                $$ = concatenate(stylize($1, "keyword"), $2);
                                $$ = concatenate($$, $3);
                                $$ = concatenate($$, $4);
                              }
or_test: and_test or_test_more { $$ = concatenate($1, $2);}
or_test_more: OR and_test or_test_more {
                $$ = concatenate(stylize($1, "operator"), $2);
                $$ = concatenate($$, $3);
              }
              | { $$ = ""; }
and_test: not_test and_test_more { $$ = concatenate($1, $2);}
and_test_more: AND not_test and_test_more {
                $$ = concatenate(stylize($1, "operator"), $2);
                $$ = concatenate($$, $3);
              }
              | { $$ = ""; }
not_test: NOT not_test { $$ = concatenate(stylize($1, "operator"), $2);}
        | comparison { $$ = $1; }
comparison: expr { $$ = $1; }
          | expr comp_more { $$ = concatenate($1, $2);}
comp_more: comp_op expr { $$ = concatenate($1, $2);}
         | comp_more comp_op expr {
                         $$ = concatenate($1, $2);
                         $$ = concatenate($$, $3);
                       }
comp_op: LESS { $$ = stylize($1, "operator");}
       | BIGGER { $$ = stylize($1, "operator");}
       | EQL { $$ = stylize($1, "operator");}
       | LESSEQL { $$ = stylize($1, "operator");}
       | BIGGEREQL { $$ = stylize($1, "operator");}
       | NOTEQL { $$ = stylize($1, "operator");}
       | IN { $$ = stylize($1, "operator");}
       | NOTIN { $$ = stylize($1, "operator");}
       | IS { $$ = stylize($1, "operator");}
       | ISNOT { $$ = stylize($1, "operator");}
star_expr: STAR expr { $$ = concatenate(stylize($1, "operator"), $2);}
expr: xor_expr { $$ = $1; }
    | xor_expr expr_more { $$ = concatenate($1, $2);}
expr_more: BOOLOR xor_expr { $$ = concatenate(stylize($1, "operator"), $2);}
    | expr_more BOOLOR xor_expr {
                    $$ = concatenate($1, stylize($2, "operator"));
                    $$ = concatenate($$, $3);
                  }
xor_expr: and_expr { $$ = $1; }
        | and_expr xor_expr_more { $$ = concatenate($1, $2);}
xor_expr_more: HAT and_expr { $$ = concatenate(stylize($1, "operator"), $2);}
             | xor_expr_more HAT and_expr {
                             $$ = concatenate($1, stylize($2, "operator"));
                             $$ = concatenate($$, $3);
                           }
and_expr: shift_expr { $$ = $1; }
        | shift_expr and_expr_more { $$ = concatenate($1, $2);}
and_expr_more: BOOLAND shift_expr { $$ = concatenate(stylize($1, "operator"), $2);}
             | and_expr_more BOOLAND shift_expr {
                             $$ = concatenate($1, stylize($2, "operator"));
                             $$ = concatenate($$, $3);
                           }
shift_expr: arith_expr { $$ = $1; }
          | arith_expr shift_expr_more { $$ = concatenate($1, $2);}
shift_expr_more: shift_op arith_expr { $$ = concatenate($1, $2);}
               | shift_expr shift_op arith_expr {
                               $$ = concatenate($1, $2);
                               $$ = concatenate($$, $3);
                             }
shift_op: SHIFTLESS { $$ = stylize($1, "operator");}
        | SHIFTRIGHT { $$ = stylize($1, "operator");}
arith_expr: term { $$ = $1; }
          | term arith_expr_more { $$ = concatenate($1, $2);}
arith_expr_more: arith_op term { $$ = concatenate($1, $2);}
               | arith_expr_more arith_op term {
                               $$ = concatenate($1, $2);
                               $$ = concatenate($$, $3);
                             }
arith_op: PLUS { $$ = stylize($1, "operator");}
        | MINUS { $$ = stylize($1, "operator");}
term: factor { $$ = $1; }
    | factor term_more { $$ = concatenate($1, $2);}
term_more: term_op factor { $$ = concatenate($1, $2);}
         | term term_op factor {
                         $$ = concatenate($1, $2);
                         $$ = concatenate($$, $3);
                       }
term_op: STAR { $$ = stylize($1, "operator");}
       | AT { $$ = stylize($1, "operator");}
       | SLASH { $$ = stylize($1, "operator");}
       | PERCENT { $$ = stylize($1, "operator");}
       | DOUBLESLASH { $$ = stylize($1, "operator");}
factor: unar_op factor { $$ = concatenate($1, $2); }
      | power { $$ = $1; }
unar_op: PLUS { $$ = stylize($1, "operator");}
       | MINUS { $$ = stylize($1, "operator");}
       | TILDA { $$ = stylize($1, "operator");}
power: atom_expr { $$ = $1; }
     | atom_expr DOUBLESTAR factor { $$ = concatenate3($1, stylize($2, "operator"), $3);}
atom_expr: AWAIT atom { $$ = concatenate(stylize($1, "operator"), $2);}
         | atom { $$ = $1; }
         | AWAIT atom trailer_more {
                         $$ = concatenate(stylize($1, "keyword"), $2);
                         $$ = concatenate($$, $3);
                       }
         | atom trailer_more { $$ = concatenate($1, $2);}
atom: LPAREN RPAREN { $$ = concatenate($1, $2);}
      | LPAREN yield_expr RPAREN { $$ = concatenate3($1, $2, $3);}
      | LPAREN testlist_comp RPAREN { $$ = concatenate3($1, $2, $3);}
      | LSQPAREN RSQPAREN { $$ = concatenate($1, $2);}
      | LSQPAREN testlist_comp RSQPAREN { $$ = concatenate3($1, $2, $3); }
      | LCURPAREN RCURPAREN { $$ = concatenate($1, $2);}
      | LCURPAREN dictorsetmaker RCURPAREN { $$ = concatenate3($1, $2, $3); }
      | NAME  { $$ = stylize($1, "identifier");}
      | NUMBER { $$ = stylize($1, "number");}
      | STRING { $$ = stylize($1, "string");}
      | THREEDOTS { $$ = stylize($1, "operator");}
      | NONE { $$ = stylize($1, "constant");}
      | TRUE { $$ = stylize($1, "constant");}
      | FALSE { $$ = stylize($1, "constant");}
      | DOLAR { $$ = "";}
testlist_comp: test testlist_comp_second { $$ = concatenate($1, $2);}
             | star_expr testlist_comp_second { $$ = concatenate($1, $2);}
testlist_comp_second: comp_for { $$ = $1; }
                    | testlist_more_arg { $$ = $1; }
testlist_more_arg: COMMA test COMMA {
                $$ = concatenate($1, $2);
                $$ = concatenate($$, $3);
              }
                  | COMMA test { $$ = concatenate($1, $2);}
                  | testlist_more_arg COMMA test COMMA {
                                  $$ = concatenate($1, $2);
                                  $$ = concatenate($$, $3);
                                  $$ = concatenate($$, $4);
                                }
                  | testlist_more_arg COMMA test {
                                  $$ = concatenate($1, $2);
                                  $$ = concatenate($$, $3);
                                }
                  | testlist_more_arg COMMA star_expr COMMA {
                                  $$ = concatenate($1, $2);
                                  $$ = concatenate($$, $3);
                                  $$ = concatenate($$, $4);
                                }
                  | testlist_more_arg COMMA star_expr {
                                  $$ = concatenate($1, $2);
                                  $$ = concatenate($$, $3);
                                }
trailer: LPAREN RPAREN { $$ = concatenate($1, $2);}
        | LPAREN arglist RPAREN {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, $3);
                      }
        | LSQPAREN subscriptlist RSQPAREN {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, $3);
                      }
        | DOT NAME { $$ = concatenate($1, stylize($2, "identifier"));}
trailer_more: trailer { $$ = $1; }
            | trailer_more trailer { $$ = concatenate($1, $2);}
subscriptlist: subscript COMMA { $$ = concatenate($1, $2);}
              | subscript { $$ = $1; }
              | subscript subscipt_more COMMA {
                              $$ = concatenate($1, $2);
                              $$ = concatenate($$, $3);
                            }
              | subscript subscipt_more { $$ = concatenate($1, $2);}
subscipt_more: COMMA subscript { $$ = concatenate($1, $2);}
             | subscipt_more COMMA subscript {
                             $$ = concatenate($1, $2);
                             $$ = concatenate($$, $3);
                           }
subscript: test { $$ = $1; }
          | COLON { $$ = $1; }
          | COLON test { $$ = concatenate($1, $2);}
          | COLON sliceop { $$ = concatenate($1, $2);}
          | COLON test sliceop {
                          $$ = concatenate($1, $2);
                          $$ = concatenate($$, $3);
                        }
          | test COLON { $$ = concatenate($1, $2);}
          | test COLON test { $$ = concatenate($1, $2);}
          | test COLON sliceop {
                          $$ = concatenate($1, $2);
                          $$ = concatenate($$, $3);
                        }
          | test COLON test sliceop {
                          $$ = concatenate($1, $2);
                          $$ = concatenate($$, $3);
                          $$ = concatenate($$, $4);
                        }
sliceop: COLON test { $$ = concatenate($1, $2);}
       | COLON { $$ = $1; }
exprlist: exprlist_op COMMA { $$ = concatenate($1, $2);}
        | exprlist_op { $$ = $1; }
        | exprlist_op exprlist_more COMMA {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, $3);
                      }
        | exprlist_op exprlist_more { $$ = concatenate($1, $2);}
exprlist_more: COMMA exprlist_op { $$ = concatenate($1, $2);}
             | exprlist_more COMMA exprlist_op {
                             $$ = concatenate($1, $2);
                             $$ = concatenate($$, $3);
                           }
exprlist_op: expr { $$ = $1; }
           | star_expr { $$ = $1; }
testlist: test { $$ = $1; }
        | test COMMA { $$ = concatenate($1, $2);}
        | test testlist_more { $$ = concatenate($1, $2);}
        | test testlist_more COMMA {
                        $$ = concatenate($1, $2);
                        $$ = concatenate($$, $3);
                      }
testlist_more: COMMA test { $$ = concatenate($1, $2);}
             | testlist_more COMMA test {
                             $$ = concatenate($1, $2);
                             $$ = concatenate($$, $3);
                           }
dictorsetmaker: test_star_expr dsm_one { $$ = concatenate($1, $2);}
              | test_star_expr  COMMA { $$ = concatenate($1, $2);} /* in the case test_star_expr is zero times */
              | test_star_expr   { $$ = $1; }     /* in the case test_star_expr is zero times */
              | test_star_expr_2 dsm_two { $$ = concatenate($1, $2);}
              | test_star_expr_2 COMMA  { $$ = concatenate($1, $2);}/* in the case test_star_expr_2 is zero times */
              | test_star_expr_2   { $$ = $1; }    /* in the case test_star_expr_2 is zero times */
test_star_expr: test COLON test {
                $$ = concatenate($1, $2);
                $$ = concatenate($$, $3);
              }
              | DOUBLESTAR expr { $$ = concatenate(stylize($1, "operator"), $2);}
dsm_one: comp_for { $$ = $1; }
       | comp_for COMMA { $$ = concatenate($1, $2);}
       | test_star_expr_more COMMA { $$ = concatenate($1, $2);}
       | test_star_expr_more { $$ = $1; }
test_star_expr_more: COMMA test_star_expr { $$ = concatenate($1, $2);}
                   | test_star_expr_more COMMA test_star_expr {
                                   $$ = concatenate($1, $2);
                                   $$ = concatenate($$, $3);
                                 }
test_star_expr_2: test { $$ = $1; }
                | star_expr { $$ = $1; }
dsm_two: comp_for { $$ = $1; }
       | comp_for COMMA { $$ = concatenate($1, $2);}
       | test_star_expr_2_more COMMA { $$ = concatenate($1, $2);}
       | test_star_expr_2_more { $$ = $1; }
test_star_expr_2_more: COMMA test_star_expr_2 { $$ = concatenate($1, $2);}
                     | test_star_expr_2_more COMMA test_star_expr_2 {
                                     $$ = concatenate($1, $2);
                                     $$ = concatenate($$, $3);
                                   }

classdef: CLASS NAME COLON suite {
                $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));
                $$ = concatenate($$, $3);
                $$ = concatenate($$, $4);
              }
        | CLASS NAME LPAREN RPAREN COLON suite {
                        $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));
                        $$ = concatenate($$, $3);
                        $$ = concatenate($$, $4);
                        $$ = concatenate($$, $5);
                        $$ = concatenate($$, $6);
                      }
        | CLASS NAME LPAREN arglist RPAREN COLON suite {
                        $$ = concatenate(stylize($1, "keyword"), stylize($2, "identifier"));
                        $$ = concatenate($$, $3);
                        $$ = concatenate($$, $4);
                        $$ = concatenate($$, $5);
                        $$ = concatenate($$, $6);
                        $$ = concatenate($$, $7);
                      }
arglist: argument COMMA { $$ = concatenate($1, $2);}
       | argument { $$ = $1; }
       | argument arglist_more COMMA {
                       $$ = concatenate($1, $2);
                       $$ = concatenate($$, $3);
                     }
       | argument arglist_more { $$ = concatenate($1, $2);}
arglist_more: COMMA argument { $$ = concatenate($1, $2);}
            | arglist_more COMMA argument {
                            $$ = concatenate($1, $2);
                            $$ = concatenate($$, $3);
                          }
argument: test comp_for { $$ = concatenate($1, $2);}
        | test { $$ = $1; }
        | test ASSIGN test {
                        $$ = concatenate($1, stylize($2, "operator"));
                        $$ = concatenate($$, $3);
                      }
        | DOUBLESTAR test { $$ = concatenate(stylize($1, "operator"), $2);}
        | STAR test { $$ = concatenate(stylize($1, "operator"), $2);}
comp_iter: comp_for { $$ = $1; }
         | comp_if { $$ = $1; }
sync_comp_for: FOR exprlist IN or_test {
                $$ = concatenate(stylize($1, "keyword"), $2);
                $$ = concatenate($$, stylize($3, "operator"));
                $$ = concatenate($$, $4);
              }
             | FOR exprlist IN or_test comp_iter {
                             $$ = concatenate(stylize($1, "keyword"), $2);
                             $$ = concatenate($$, stylize($3, "operator"));
                             $$ = concatenate($$, $4);
                             $$ = concatenate($$, $5);
                           }
comp_for: sync_comp_for { $$ = $1; }
        | ASYNC comp_for { $$ = concatenate(stylize($1, "keyword"), $2);}
comp_if: IF test_nocond { $$ = concatenate(stylize($1, "keyword"), $2);}
       | IF test_nocond comp_iter {
                       $$ = concatenate(stylize($1, "keyword"), $2);
                       $$ = concatenate($$, $3);
                     }

/* encoding_decl: NAME */

yield_expr: YIELD { $$ = stylize($1, "keyword");}
          | YIELD yield_arg { $$ = concatenate(stylize($1, "keyword"), $2);}
yield_arg: FROM test { $$ = concatenate(stylize($1, "keyword"), $2);}
         | testlist { $$ = $1; }

%%
int yyerror(const char* error)
{
    extern int current_line;
    fprintf(stderr, "input:%d: error: %s\n", current_line, error);
    return 0;
}

void printHtmlHeader(){
       puts(
                   "<head>"
                   "    <title>hello_world.go</title>"
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
