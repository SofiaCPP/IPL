%option nounistd
%option noyywrap
%option stack

%{
#include <stdbool.h>

#define INDENTATION "    "
#define SPACE " "
#define HIGHLIGHT_TERMINATOR "</span>"

#define PRINT_HIGHLIGHTED_TOKEN_INITIATOR(type) \
    printf("<span class=\"" #type "\">%s", yytext)

#define PRINT_HIGHLIGHTED_TOKEN_TERMINATOR \
    printf("%s" HIGHLIGHT_TERMINATOR, yytext)

#define PRINT_HIGHLIGHTED_TOKEN(type) \
    printf("<span class=\"" #type "\">%s</span>", yytext)

#define PRINT_TOKEN \
    printf("%s", yytext)

#define PRINT_INDENTED_NEWLINE \
    printf("\n"); \
    for (int i = 0; i < indentation_level; i++) \
        printf(INDENTATION)

#define CLEAR_MODES \
    require_space_mode = false; \
    adjacent_parenthesis_mode = false

#define HANDLE_COMMENT_INITIATOR(type) \
    PRINT_HIGHLIGHTED_TOKEN_INITIATOR(comment); \
    yy_push_state(type); \
    CLEAR_MODES

#define HANDLE_LITERAL(type) \
    if (require_space_mode) \
        printf(SPACE); \
    PRINT_HIGHLIGHTED_TOKEN(type); \
    require_space_mode = true; \
    adjacent_parenthesis_mode = false

#define HANDLE_IDENTIFIER \
    if (require_space_mode) \
        printf(SPACE); \
    PRINT_HIGHLIGHTED_TOKEN(identifier); \
    require_space_mode = true; \
    adjacent_parenthesis_mode = true

#define HANDLE_EOL \
    yy_pop_state(); \
    adjacent_parenthesis_mode = true; \
    yyless(0)

static int indentation_level = 0;
static bool require_space_mode = false;
static bool adjacent_parenthesis_mode = false;
%}

%s multiline_list
%s path
%s directive

%x line_comment
%x block_comment
%x eol

NONEOL_CHAR        .
DEC_DIGIT          0|{NONZERO_DEC_DIGIT}
NONZERO_DEC_DIGIT  [1-9]
OCT_DIGIT          [0-7]
HEX_DIGIT          {DEC_DIGIT}|[a-fA-F]

IDENTIFIER             {IDENTIFIER_FIRST_CHAR}({DEC_DIGIT}|{IDENTIFIER_FIRST_CHAR})*
IDENTIFIER_FIRST_CHAR  [A-Za-z_]

LINE_COMMENT_BEGIN        "//"
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

%%

<eol>[[:space:]]+

<eol>";"|"}" {
    HANDLE_EOL;
}

<eol>"else" {
    HANDLE_EOL;
    printf(SPACE);
}

<eol>. {
    HANDLE_EOL;
    PRINT_INDENTED_NEWLINE;
}

<line_comment>[[:blank:]]*\n {
    yy_pop_state();
    printf(HIGHLIGHT_TERMINATOR);
    yy_push_state(eol);
}

<block_comment>{BLOCK_COMMENT_TERMINATOR} {
    yy_pop_state();
    PRINT_HIGHLIGHTED_TOKEN_TERMINATOR;
    yy_push_state(eol);
}

<line_comment>. {
    PRINT_TOKEN;
}

<block_comment>(.|\n) {
    PRINT_TOKEN;
}

[[:space:]]+

"use" {
    HANDLE_LITERAL(keyword);
    yy_push_state(path);
}

{KEYWORD} {
    HANDLE_LITERAL(keyword);
}

{IDENTIFIER} {
    HANDLE_IDENTIFIER;
}

{NUMBER_LITERAL} {
    HANDLE_LITERAL(number);
}

{CHAR_LITERAL}|{STRING_LITERAL}|{BYTE_CHAR_LITERAL}|{BYTE_STRING_LITERAL} {
    HANDLE_LITERAL(string);
}

{LINE_COMMENT_BEGIN} {
    HANDLE_COMMENT_INITIATOR(line_comment);
}

{BLOCK_COMMENT_INITIATOR} {
    HANDLE_COMMENT_INITIATOR(block_comment);
}

<multiline_list>"," {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(eol);
}

","|":" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    require_space_mode = true;
    adjacent_parenthesis_mode = false;
}

<path>";" {
    yy_pop_state();
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(eol);
}

";" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(eol);
}

"#" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(directive);
}

"?" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    require_space_mode = true;
    adjacent_parenthesis_mode = false;
}

"!" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    require_space_mode = false;
    adjacent_parenthesis_mode = true;
}

"."|"::"|"'"|"$"|"@"|"~" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
}

"=="|"=>"|"="|"!="|"<="|"<<"|"<<="|"<"|">="|">>"|">>="|">"|"<-"|"->"|"-"|"-="|"&&"|"&"|"&="|"||"|"|"|"|="|"+"|"+="|"*"|"*="|"/"|"/="|"^"|"^="|"%"|"%="|".."|"..." {
    printf(SPACE);
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    require_space_mode = true;
    adjacent_parenthesis_mode = false;
}

"(" {
    if (!adjacent_parenthesis_mode)
        printf(SPACE);
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(INITIAL);
}

<directive>"[" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
}

"[" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(INITIAL);
}

<directive>"]" {
    yy_pop_state();
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(eol);
}

")"|"]" {
    yy_pop_state();
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    require_space_mode = true;
    adjacent_parenthesis_mode = false;
}

<path>"{" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
}

"{" {
    if (require_space_mode)
        printf(SPACE);
    indentation_level++;
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
    yy_push_state(multiline_list);
    yy_push_state(eol);
}

<path>"}" {
    yy_pop_state();
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    CLEAR_MODES;
}

"}" {
    yy_pop_state();
    indentation_level--;
    PRINT_INDENTED_NEWLINE;
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    if (0 == indentation_level)
        printf("\n");
    CLEAR_MODES;
    yy_push_state(eol);
}

. {
    PRINT_TOKEN;  /* echo the rest */
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
