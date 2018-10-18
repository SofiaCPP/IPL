/* scanner for the Rust programming language */

%{
#define YY_NO_UNISTD_H
%}

NONEOL_CHAR        [^\n]
DEC_DIGIT          0|{NONZERO_DEC_DIGIT}
NONZERO_DEC_DIGIT  [1-9]
OCT_DIGIT          [0-7]
HEX_DIGIT          {DEC_DIGIT}|[a-fA-F]

IDENTIFIER             {IDENTIFIER_FIRST_CHAR}({DEC_DIGIT}|{IDENTIFIER_FIRST_CHAR})*
IDENTIFIER_FIRST_CHAR  [A-Za-z_]

COMMENT        {BLOCK_COMMENT}|{LINE_COMMENT}
BLOCK_COMMENT  "/*".*"*/"
LINE_COMMENT   "//"{NONEOL_CHAR}*

KEYWORD  _|abstract|alignof|as|become|box|break|const|continue|crate|do|else|enum|extern|false|final|fn|for|if|impl|in|let|loop|macro|match|mod|move|mut|offsetof|override|priv|proc|pub|pure|ref|return|Self|self|sizeof|static|struct|super|trait|true|type|typeof|unsafe|unsized|use|virtual|where|while|yield

ESCAPE          {COMMON_ESCAPE}|{UNICODE_ESCAPE}
COMMON_ESCAPE   \\|n|r|t|0|x{HEX_DIGIT}{2}
UNICODE_ESCAPE  "u{"{HEX_DIGIT}{6}"}"

CHAR_LITERAL         '{CHAR_BODY}'
CHAR_BODY            [^']|(\\('|{ESCAPE}))
STRING_LITERAL       \"{STRING_BODY}*\"
STRING_BODY          [^\"]|(\\(\"|{ESCAPE}))
BYTE_CHAR_LITERAL    b'{BYTE_CHAR_BODY}'
BYTE_CHAR_BODY       [^']|(\\('|{COMMON_ESCAPE}))
BYTE_STRING_LITERAL  b\"{BYTE_STRING_BODY}*\"
BYTE_STRING_BODY     [^\"]|(\\(\"|{ESCAPE}))

NUMBER_LITERAL     {NONZERO_DEC_DIGIT}{DEC_DIGIT}*{FLOAT_SUFFIX}?|(0{DEC_DIGIT_LITERAL}*{FLOAT_SUFFIX}?|b[10_]+|o{OCT_DIGIT_LITERAL}+|x{HEX_DIGIT_LITERAL}+)
FLOAT_SUFFIX       {EXPONENT}|"."{DEC_DIGIT_LITERAL}+{EXPONENT}?
EXPONENT           [Ee][-+]?{DEC_DIGIT_LITERAL}+
DEC_DIGIT_LITERAL  {DEC_DIGIT}|_
HEX_DIGIT_LITERAL  {HEX_DIGIT}|_
OCT_DIGIT_LITERAL  {OCT_DIGIT}|_

SYMBOL  "::"|"->"|"#"|"["|"]"|"("|")"|"{"|"}"|","|";"

%%

{SYMBOL} {
    printf("<span class=\"symbol\">%s</span>", yytext);
}

{KEYWORD} {
    printf("<span class=\"keyword\">%s</span>", yytext);
}

{NUMBER_LITERAL} {
    printf("<span class=\"number\">%s</span>", yytext);
}

{IDENTIFIER} {
    printf("<span class=\"identifier\">%s</span>", yytext);
}

{CHAR_LITERAL}|{STRING_LITERAL}|{BYTE_CHAR_LITERAL}|{BYTE_STRING_LITERAL} {
    printf("<span class=\"string\">%s</span>", yytext);
}

{COMMENT} {
    printf("<span class=\"comment\">%s</span>", yytext);
}

. printf("%s", yytext);  /* echo the rest */

%%

int yywrap()
{
    return 1;
}

int main(int argc, const char* argv[])
{
    yyin = argc > 1 ? fopen(argv[1], "r") : stdin;

    puts(
        "<!doctype>"
        "<html>"
        "<head>"
        "    <title>hello.js</title>"
        "    <style>"
        "        .comment {"
        "            color: gray;"
        "        }"
        "        .keyword {"
        "            color: red;"
        "        }"
        "        .string {"
        "            color: green;"
        "        }"
        "        .number {"
        "            color: blue;"
        "        }"
        "        .symbol {"
        "            font-style: bold;"
        "        }"
        "    </style>"
        "</head>"
        "<body>"
        "    <pre class=\"code\">"
    );
    yylex();
    puts("</pre></body></html>");

    return 0;
}
