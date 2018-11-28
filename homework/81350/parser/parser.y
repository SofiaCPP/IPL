
%code requires {
#include <stdio.h>
#include <string.h>
int yylex();
int yyerror(const char* error);
FILE *yyin;
#undef YYDEBUG
#define YYDEBUG 1
/* #define YY_DECL int yylex(scanner) void* scanner; */
}

%start input_stmt
/* %define api.pure full */
%define api.value.type {union name { double Number; char Lexeme[256];} }

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

/*
Define both - precedence and associativity */
%left PLUS MINUS
%left STAR SLASH

%%
/*
input:
        | expr  { printf("%f\n", $1.Number); }
        ;
expr:   NUMBER  { $$.Number = $1.Number; }
        | '(' expr ')' { $$.Number = $2.Number; }
        | expr PLUS expr { $$.Number = $1.Number + $3.Number; }
        | expr MINUS expr { $$.Number = $1.Number - $3.Number; }
        | expr STAR expr { $$.Number = $1.Number * $3.Number; }
        | expr SLASH expr { $$.Number = $1.Number / $3.Number; }
*/

input_stmt: NEWLINE | simple_stmt | compound_stmt NEWLINE | compound_stmt


decorator: AT dotted_name NEWLINE
         | AT dotted_name LPAREN RPAREN NEWLINE
         | AT dotted_name LPAREN arglist RPAREN NEWLINE
decorator_more: decorator | decorator_more decorator
decorated: decorator_more obj_definition
obj_definition: classdef | funcdef | async_funcdef


async_funcdef: ASYNC funcdef
funcdef: DEF NAME parameters COLON suite /* | DEF NAME parameters '->' test COLON suite */
parameters: LPAREN RPAREN | LPAREN typedargslist RPAREN

typedargslist: td_one | td_two | td_three
td_one: td_one_arg td_more_arg td_one_optional | td_one_arg td_one_optional
td_one_arg: tfpdef ASSIGN test | tfpdef
td_more_arg: COMMA td_one_arg | td_more_arg COMMA td_one_arg
/* td_add_arg: COMMA td_one_arg td_more_arg */
td_one_optional: COMMA td_two | COMMA td_three | COMMA |
td_two: STAR tfpdef td_more_arg td_two_optional | STAR td_more_arg td_two_optional
td_two_optional: COMMA td_three | COMMA
td_three: DOUBLESTAR tfpdef COMMA | DOUBLESTAR tfpdef
tfpdef: NAME | NAME COLON test

varargslist: var_one | var_two | var_three
var_one: var_one_arg var_more_arg var_one_optional | var_one_arg var_one_optional
var_one_arg: vfpdef ASSIGN test | vfpdef
var_more_arg: COMMA var_one_arg | var_more_arg COMMA var_one_arg
var_one_optional: COMMA var_two | COMMA var_three | COMMA
var_two: STAR vfpdef var_more_arg var_two_optional | STAR var_more_arg var_two_optional
var_two_optional: COMMA var_three | COMMA
var_three: DOUBLESTAR vfpdef COMMA | DOUBLESTAR vfpdef
vfpdef: NAME

stmt: simple_stmt
    | compound_stmt
simple_stmt: small_stmt simple_more_stmt SEMICOLON NEWLINE
            | small_stmt simple_more_stmt NEWLINE
            | small_stmt NEWLINE
            | small_stmt SEMICOLON NEWLINE
simple_more_stmt: SEMICOLON small_stmt
                | simple_more_stmt SEMICOLON small_stmt
small_stmt: expr_stmt
          | del_stmt
          | pass_stmt
          | flow_stmt
          | import_stmt
          | global_stmt
          | nonlocal_stmt
          | assert_stmt
expr_stmt: testlist_star_expr expr_stmt_two
         | testlist_star_expr
expr_stmt_two: annassign
             | augassign aug_expr
             | assign_more
assign_more: ASSIGN assign_expr
           | assign_more ASSIGN assign_expr
assign_expr: yield_expr
           | testlist_star_expr
aug_expr: yield_expr
        | testlist
annassign: COLON test | COLON test ASSIGN test
testlist_star_expr: testlist_se testlist_star_expr_more COMMA
                  | testlist_se testlist_star_expr_more
                  | testlist_se COMMA
                  | testlist_se
testlist_se: test
           | star_expr
testlist_star_expr_more: COMMA testlist_se | testlist_star_expr_more COMMA testlist_se
augassign: PLUSASSIGN
         | MINUSASSIGN
         | STARASSIGN
         | ATASSIGN
         | SLASHASSIGN
         | PERCENTASSIGN
         | BOOLANDASSIGN
         | BOOLORASSIGN
         | HATASSIGN
         | SHIFTLEFTASSIGN
         | SHIFTRIGHTASSIGN
         | DOUBLESTARASSIGN
         | DOUBLESLASHASSIGN

del_stmt: DEL exprlist
pass_stmt: PASS
flow_stmt: break_stmt
         | continue_stmt
         | return_stmt
         | raise_stmt
         | yield_stmt
break_stmt: BREAK
continue_stmt: CONTINUE
return_stmt: RETURN | RETURN testlist
yield_stmt: yield_expr
raise_stmt: RAISE | RAISE test | RAISE test FROM test
import_stmt: import_name | import_from
import_name: IMPORT dotted_as_names
import_from: FROM import_from_name IMPORT import_from_import
import_from_name: dotted_name | dots_more dotted_name | dots_more
dots: DOT | THREEDOTS
dots_more: dots | dots_more dots
import_from_import: STAR
                  | LPAREN import_as_names RPAREN
                  | import_as_names
import_as_name: NAME | NAME AS NAME
dotted_as_name: dotted_name | dotted_name AS NAME
import_as_names: import_as_name COMMA
               | import_as_name
               | import_as_name import_as_name_more COMMA
               | import_as_name import_as_name_more
import_as_name_more: COMMA import_as_name
                   | import_as_name_more COMMA import_as_name
dotted_as_names: dotted_as_name
               | dotted_as_name dotted_as_name_more
dotted_as_name_more: COMMA dotted_as_name
                   | dotted_as_name_more COMMA dotted_as_name
dotted_name: NAME | NAME dotted_name_more
dotted_name_more: DOT NAME | dotted_name_more DOT NAME
global_stmt: GLOBAL NAME | GLOBAL NAME global_stmt_more
global_stmt_more: COMMA NAME | global_stmt_more COMMA NAME
nonlocal_stmt: NONLOCAL NAME | NONLOCAL NAME nonlocal_stmt_more
nonlocal_stmt_more: COMMA NAME | nonlocal_stmt_more COMMA NAME
assert_stmt: ASSERT test | ASSERT COMMA test

compound_stmt: if_stmt
             | while_stmt
             | for_stmt
             | try_stmt
             | with_stmt
             | funcdef
             | classdef
             | decorated
             | async_stmt
async_stmt: ASYNC funcdef | ASYNC with_stmt | ASYNC for_stmt
if_stmt: IF test COLON suite elif_stmt_more else_stmt
       | IF test COLON suite elif_stmt_more
       | IF test COLON suite else_stmt
       | IF test COLON suite
elif_stmt: ELIF test COLON suite
else_stmt: ELSE COLON suite
elif_stmt_more: elif_stmt | elif_stmt_more elif_stmt
while_stmt: WHILE test COLON suite
          | WHILE test COLON suite else_stmt
for_stmt: FOR exprlist IN testlist COLON suite
        | FOR exprlist IN testlist COLON suite else_stmt
try_stmt: TRY COLON suite try_stmt_two
try_stmt_two: except_fin_stmt | finally_stmt
finally_stmt: FINALLY COLON suite
except_fin_stmt: except_stmt_more
               | except_stmt_more else_stmt
               | except_stmt_more finally_stmt
               | except_stmt_more else_stmt finally_stmt
except_stmt_more: except_stmt | except_stmt_more except_stmt
except_stmt: except_clause COLON suite
except_clause: EXCEPT
             | EXCEPT test
             | EXCEPT test AS NAME
with_stmt: WITH with_item with_item_more COLON suite
         | WITH with_item COLON suite
with_item: test | test AS expr
with_item_more: COMMA with_item | with_item_more COMMA with_item
suite: simple_stmt | NEWLINE INDENT stmt_more UNINDENT
stmt_more: stmt | stmt_more stmt


test: or_test
    | or_test IF or_test ELSE test
    | lambdadef
test_nocond: or_test | lambdadef_nocond
lambdadef: LAMBDA COLON test
         | LAMBDA varargslist COLON test
lambdadef_nocond: LAMBDA COLON test_nocond
                | LAMBDA varargslist COLON test
or_test: and_test or_test_more
or_test_more: | OR and_test or_test_more
and_test: not_test and_test_more
and_test_more: | AND not_test and_test_more
not_test: NOT not_test | comparison
comparison: expr | expr comp_more
comp_more: comp_op expr | comp_more comp_op expr
comp_op: LESS | BIGGER | EQL | LESSEQL | BIGGEREQL | NOTEQL | IN | NOTIN | IS | ISNOT
star_expr: STAR expr
expr: xor_expr | xor_expr expr_more
expr_more: BOOLOR xor_expr | expr BOOLOR xor_expr
xor_expr: and_expr | and_expr xor_expr_more
xor_expr_more: HAT and_expr | xor_expr HAT and_expr
and_expr: shift_expr | shift_expr and_expr_more
and_expr_more: BOOLAND shift_expr | and_expr BOOLAND shift_expr
shift_expr: arith_expr | arith_expr shift_expr_more
shift_expr_more: shift_op arith_expr | shift_expr shift_op arith_expr
shift_op: SHIFTLESS | SHIFTRIGHT
arith_expr: term | term arith_expr_more
arith_expr_more: arith_op term | arith_expr arith_op term
arith_op: PLUS | MINUS
term: factor | factor term_more
term_more: term_op factor | term term_op factor
term_op: STAR | AT | SLASH | PERCENT | DOUBLESLASH
factor: unar_op factor | power
unar_op: PLUS | MINUS | TILDA
power: atom_expr | atom_expr DOUBLESTAR factor
atom_expr: AWAIT atom | atom | AWAIT atom trailer_more | atom trailer_more
atom: LPAREN RPAREN
      | LPAREN yield_expr RPAREN
      | LPAREN testlist_comp RPAREN
      | LSQPAREN RSQPAREN
      | LSQPAREN testlist_comp RSQPAREN
      | LCURPAREN RCURPAREN
      | LCURPAREN dictorsetmaker RCURPAREN
      | NAME
      | NUMBER
      | STRING
      | THREEDOTS
      | NONE
      | TRUE
      | FALSE
testlist_comp: test testlist_comp_second | star_expr testlist_comp_second
testlist_comp_second: comp_for | testlist_more_arg
testlist_more_arg: COMMA test COMMA
                  | COMMA test
                  | testlist_more_arg COMMA test COMMA
                  | testlist_more_arg COMMA test
                  | testlist_more_arg COMMA star_expr COMMA
                  | testlist_more_arg COMMA star_expr
trailer: LPAREN RPAREN
        | LPAREN arglist RPAREN
        | LSQPAREN subscriptlist RSQPAREN
        | DOT NAME
trailer_more: trailer | trailer_more trailer
subscriptlist: subscript COMMA
              | subscript
              | subscript subscipt_more COMMA |
              | subscript subscipt_more
subscipt_more: COMMA subscript | subscipt_more COMMA subscript
subscript: test
          | COLON
          | COLON test
          | COLON sliceop
          | COLON test sliceop
          | test COLON
          | test COLON test
          | test COLON sliceop
          | test COLON test sliceop
sliceop: COLON test | COLON
exprlist: exprlist_op COMMA | exprlist_op | exprlist_op exprlist_more COMMA | exprlist_op exprlist_more
exprlist_more: COMMA exprlist_op | exprlist_more COMMA exprlist_op
exprlist_op: expr | star_expr
testlist: test | test COMMA | test testlist_more | test testlist_more COMMA
testlist_more: COMMA test | testlist_more COMMA test
dictorsetmaker: test_star_expr dsm_one
              | test_star_expr  COMMA /* in the case test_star_expr is zero times */
              | test_star_expr        /* in the case test_star_expr is zero times */
              | test_star_expr_2 dsm_two
              | test_star_expr_2 COMMA /* in the case test_star_expr_2 is zero times */
              | test_star_expr_2       /* in the case test_star_expr_2 is zero times */
test_star_expr: test COLON test | DOUBLESTAR expr
dsm_one: comp_for | comp_for COMMA | test_star_expr_more COMMA | test_star_expr_more
test_star_expr_more: COMMA test_star_expr | test_star_expr_more COMMA test_star_expr
test_star_expr_2: test | star_expr
dsm_two: comp_for | comp_for COMMA | test_star_expr_2_more COMMA | test_star_expr_2_more
test_star_expr_2_more: COMMA test_star_expr_2 | test_star_expr_2_more COMMA test_star_expr_2

classdef: CLASS NAME COLON suite
        | CLASS NAME LPAREN RPAREN COLON suite
        | CLASS NAME LPAREN arglist RPAREN COLON suite
arglist: argument COMMA
       | argument
       | argument arglist_more COMMA
       | argument arglist_more
arglist_more: COMMA argument
            | arglist_more COMMA argument
argument: test comp_for
        | test
        | test ASSIGN test
        | DOUBLESTAR test
        | STAR test
comp_iter: comp_for
         | comp_if
sync_comp_for: FOR exprlist IN or_test
             | FOR exprlist IN or_test comp_iter
comp_for: sync_comp_for
        | ASYNC comp_for
comp_if: IF test_nocond | IF test_nocond comp_iter

/* encoding_decl: NAME */

yield_expr: YIELD | YIELD yield_arg
yield_arg: FROM test | testlist

%%
int yyerror(const char* error)
{
    extern int current_line;
    fprintf(stderr, "input:%d: error: %s\n", current_line, error);
    return 0;
}
 int main()
{
#if YYDEBUG
    yydebug = 1;
#endif
FILE *myfile = fopen("input.file", "r");
if (!myfile) {
  printf("I can't open input.file!\n");
  return -1;
}
yyin = myfile;
yyparse();
    return 0;
}
