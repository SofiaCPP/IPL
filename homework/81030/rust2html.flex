/* scanner for the Rust programming language */

%{
#define YY_NO_UNISTD_H
%}

DIGIT          [0-9]
NONZERO_DIGIT  [1-9]
FLOAT_SUFFIX   ([Ee][-+]?[0-9_]+)|\.[0-9_]+([Ee][-+]?[0-9_]+)?
IDENTIFIER     [A-Za-z_][0-9A-Za-z_]*
ESCAPE         \\(\\|n|r|t|0|x[a-fA-F0-9][a-fA-F0-9]|u\{[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\})
BYTE_CHAR      b'([^']|(\\\'|n|r|t|0|x[a-fA-F0-9][a-fA-F0-9]))'
BYTE_STRING    b\"([^\"]|(\\\"|n|r|t|0|x[a-fA-F0-9][a-fA-F0-9]))*\"
BLOCK_COMMENT  "/*".*"*/"
LINE_COMMENT   "//"[^\n]*

%%

{NONZERO_DIGIT}({DIGIT}|_)*{FLOAT_SUFFIX}?|(0(({DIGIT}|_)*{FLOAT_SUFFIX}?|b[10_]+|o[0-7_]+|x[a-fA-F0-9_]+)) {
    printf("<span class=\"number\">%s</span>", yytext);
}

_|abstract|alignof|as|become|box|break|const|continue|crate|do|else|enum|extern|false|final|fn|for|if|impl|in|let|loop|macro|match|mod|move|mut|offsetof|override|priv|proc|pub|pure|ref|return|Self|self|sizeof|static|struct|super|trait|true|type|typeof|unsafe|unsized|use|virtual|where|while|yield {
    printf("<span class=\"keyword\">%s</span>", yytext);
}

{IDENTIFIER} {
    printf("<span class=\"identifier\">%s</span>", yytext);
}

('[^']|(\\'|{ESCAPE})')|(\"([^\"]|(\\\"|{ESCAPE}))*\")|{BYTE_CHAR}|{BYTE_STRING} {
    printf("<span class=\"string\">%s</span>", yytext);
}

{BLOCK_COMMENT}|{LINE_COMMENT} {
    printf("<span class=\"comment\">%s</span>", yytext);
}

"::"|"->"|"#"|"["|"]"|"("|")"|"{"|"}"|","|";" printf("<span class=\"symbol\">%s</span>", yytext);

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
        "        .keyword {"
        "            color: red;"
        "        }"
        "        .number {"
        "            color: blue;"
        "        }"
        "        .string {"
        "            color: green;"
        "        }"
        "        .symbol {"
        "            font-style: bold;"
        "        }"
        "        .comment {"
        "            color: gray;"
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
