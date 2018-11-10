%{
#include "Common.h"
#include "Token.h"
#include "Highlighter.h"

IPLVector<Token> tokens;
%}

LineTerminator \n|\r|\r\n

Whitespace  \t+

SingleLineComment \/\/.*

MultiLineComment \/\*{1,2}[^\*]+\*\/

Comment {SingleLineComment}|{MultiLineComment}

Keyword break|case|catch|class|const|continue|debugger|default|delete|do|else|enum|export|extends|finally|for|function|if|import|in|of|instanceof|new|return|super|switch|this|throw|try|typeof|var|void|while|with|module

BooleanLiteral true|false

DecimalDigits [0-9]+

SignedInteger {DecimalDigits}|\+{DecimalDigits}|\-{DecimalDigits}

ExponentPart e{SignedInteger}|E{SignedInteger}

DecimalLiteral {DecimalDigits}\.{DecimalDigits}?{ExponentPart}?|\.{DecimalDigits}{ExponentPart}?|{DecimalDigits}{ExponentPart}?

NumericLiteral {DecimalLiteral}

SingleQuoteString   '([^\']|(\\\'))*'

DoubleQuoteString   \"([^\"]|(\\\"))*\"

StringLiteral   {SingleQuoteString}|{DoubleQuoteString}

TemplateLiteral `([^`]|(\\`))*`

Regex   \/[^\/]+\/[gimuy]*

Identifier  [a-zA-Z_$][a-zA-Z0-9_$]*

Global  Infinity|NaN|undefined|null|eval|isNaN|parseFloat|parseInt|Object|Function|Boolean|Symbol|Error|Number|Math|Date|String|RegExp|Array|Map|Set|JSON|Promise|arguments

%%

{Whitespace} { tokens.emplace_back(Whitespace, yytext); }

{LineTerminator}   { tokens.emplace_back(NewLine, yytext); }

{Comment}   { tokens.emplace_back(Comment, yytext); }

{Keyword}   { tokens.emplace_back(Keyword, yytext); }

{BooleanLiteral} { tokens.emplace_back(BooleanLiteral, yytext); }

{NumericLiteral} { tokens.emplace_back(NumericLiteral, yytext); }

{StringLiteral} { tokens.emplace_back(StringLiteral, yytext); }

{TemplateLiteral} { tokens.emplace_back(TemplateLiteral, yytext); }

{Regex} { tokens.emplace_back(Regex, yytext); }

{Global}    { tokens.emplace_back(Global, yytext); }

{Identifier}    { tokens.emplace_back(Identifier, yytext); }

.           { tokens.emplace_back(Raw, yytext); }


%%

int yywrap()
{
    return 1;
}

int main(int argc, const char *argv[]) {
    ++argv, --argc;
    if ( argc > 0 )
        yyin = fopen( argv[0], "r" );
    else
        yyin = stdin;

    yylex();

    ++argv, --argc;
    const char *out = nullptr;
    if (argc > 0) {
        out = argv[0];
    }

    Highlighter hh(out);
    hh.highlight(tokens);
    return 0;
}