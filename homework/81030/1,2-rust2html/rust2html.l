%option nounistd
%option noyywrap

%{
#define PRINT_HIGHLIGHTED_TOKEN(type) \
    printf("<span class=\"" #type "\">%s</span>", yytext)
%}

NONEOL_CHAR        .
DEC_DIGIT          0|{NONZERO_DEC_DIGIT}
NONZERO_DEC_DIGIT  [1-9]
OCT_DIGIT          [0-7]
HEX_DIGIT          {DEC_DIGIT}|[a-fA-F]

IDENTIFIER             {IDENTIFIER_FIRST_CHAR}({DEC_DIGIT}|{IDENTIFIER_FIRST_CHAR})*
IDENTIFIER_FIRST_CHAR  [A-Za-z_]

LINE_COMMENT              "//"{NONEOL_CHAR}*
BLOCK_COMMENT_INITIATOR   "/*"
BLOCK_COMMENT_TERMINATOR  "*/"

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

SYMBOL  "::"|"->"|"#"|"["|"]"|"("|")"|"{"|"}"|","|";"|":"|"?"|"!"|"."|"'"|"$"|"@"|"~"|"=="|"=>"|"="|"!="|"<="|"<<"|"<<="|"<"|">="|">>"|">>="|">"|"<-"|"-"|"-="|"&&"|"&"|"&="|"||"|"|"|"|="|"+"|"+="|"*"|"*="|"/"|"/="|"^"|"^="|"%"|"%="|".."|"..."

%%

{SYMBOL} {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
}

{KEYWORD} {
    PRINT_HIGHLIGHTED_TOKEN(keyword);
}

{IDENTIFIER} {
    PRINT_HIGHLIGHTED_TOKEN(identifier);
}

{NUMBER_LITERAL} {
    PRINT_HIGHLIGHTED_TOKEN(number);
}

{CHAR_LITERAL}|{STRING_LITERAL}|{BYTE_CHAR_LITERAL}|{BYTE_STRING_LITERAL} {
    PRINT_HIGHLIGHTED_TOKEN(string);
}

{LINE_COMMENT} {
    PRINT_HIGHLIGHTED_TOKEN(comment);
}

{BLOCK_COMMENT_INITIATOR} {
    printf("<span class=\"comment\">%s", yytext);
}

{BLOCK_COMMENT_TERMINATOR} {
    printf("%s</span>", yytext);
}

. {
    printf("%s", yytext);  /* echo the rest */
}

%%

int main(int argc, const char* const argv[]) {
    yyin = argc > 1 ? fopen(argv[1], "r") : stdin;

    printf(
        "<!DOCTYPE html>\n"
        "<html>\n"
        "\n"
        "<head>\n"
        "    <style>\n"
        "        .comment {\n"
        "            color: gray;\n"
        "        }\n"
        "\n"
        "        .keyword {\n"
        "            color: red;\n"
        "        }\n"
        "\n"
        "        .string {\n"
        "            color: green;\n"
        "        }\n"
        "\n"
        "        .number {\n"
        "            color: blue;\n"
        "        }\n"
        "\n"
        "        .symbol {\n"
        "            font-weight: bold;\n"
        "        }\n"
        "    </style>\n"
        "</head>\n"
        "\n"
        "<body>\n"
        "    <pre class=\"code\">\n"
    );
    yylex();
    printf(
        "\n"
        "    </pre>\n"
        "</body>\n"
        "\n"
        "</html>"
    );

    return 0;
}
