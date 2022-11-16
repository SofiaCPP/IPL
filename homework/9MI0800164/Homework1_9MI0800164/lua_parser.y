%{
	#include <iostream>
	
	int yylex();
%}

%token NUMBER
%token ID
%token KEYWORD
%token OPERATOR
%token EOL

%%

line: NUMBER | ID | OPERATOR | 

%%
