%option noyywrap

%{
#ifdef _WIN32
#define isatty _isatty
#else
#include <unistd.h>
#endif

#include <vector>
#include <string>
#include <algorithm>
#include <cctype>
#include <cmath>

#define PRINT_HIGHLIGHTED_TOKEN(type) \
    printf("<span class=\"" #type "\">%s</span>", yytext)

#define GRADIENT_SHADE_RED(ratio)   (int)round(144 +  95 * ratio)
#define GRADIENT_SHADE_GREEN(ratio) (int)round(238 - 177 * ratio)
#define GRADIENT_SHADE_BLUE(ratio)  (int)round(144 -  73 * ratio)

static const char* get_fn_name(const char* name)
{
    name += 2;
    while (isspace(*name))
        name++;
    return name;
}

static int indentation_level = 0;
static int current_cyclomatic_complexity = 0;
static std::vector<int> cyclomatic_complexity_measures;
static std::vector<std::string> class_names;
%}

%s fn_def

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

<fn_def>"{" {
    indentation_level++;
    PRINT_HIGHLIGHTED_TOKEN(symbol);
}

<fn_def>"}" {
    PRINT_HIGHLIGHTED_TOKEN(symbol);
    indentation_level--;
    if (0 == indentation_level)
    {
        printf("</span>\n");
        cyclomatic_complexity_measures.push_back(current_cyclomatic_complexity);
        current_cyclomatic_complexity = 0;
        BEGIN(INITIAL);
    }
}

<fn_def>"&&"|"||"|"=>" {
    current_cyclomatic_complexity++;
    PRINT_HIGHLIGHTED_TOKEN(symbol);
}

<fn_def>"?" {
    current_cyclomatic_complexity += 2;
    PRINT_HIGHLIGHTED_TOKEN(symbol);
}

<fn_def>if|for|loop|while {
    current_cyclomatic_complexity++;
    PRINT_HIGHLIGHTED_TOKEN(keyword);
}

<INITIAL>"fn"[[:space:]]+{IDENTIFIER} {
    current_cyclomatic_complexity = 1;
    const char* name = get_fn_name(yytext);
    class_names.push_back(std::string {name});
    printf("<span class=\"fn-%s\">", name);
    yyless(0);
    BEGIN(fn_def);
}

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
        "    <style>\n"
    );
#ifdef _DEBUG
    fprintf(stderr, "Number of functions: %d\n", class_names.size());
    fprintf(stderr, "Number of cyclomatic complexity measures: %d\n", cyclomatic_complexity_measures.size());
#endif
    if (!cyclomatic_complexity_measures.empty())
    {
        int max_cyclomatic_complexity = *std::max_element(cyclomatic_complexity_measures.begin(), cyclomatic_complexity_measures.end());
#ifdef _DEBUG
        fprintf(stderr, "Maximal cyclomatic complexity: %d\n", max_cyclomatic_complexity);
#endif
        for (int i = 0; i < class_names.size(); i++)
        {
            double cyclomatic_complexity_ratio = cyclomatic_complexity_measures[i] / (double)max_cyclomatic_complexity;
#ifdef _DEBUG
            fprintf(stderr, "Cyclomatic complexity of function %s (i = %d): %d\n", class_names[i].c_str(), i, cyclomatic_complexity_measures[i]);
            fprintf(stderr, "Cyclomatic complexity ratio of function %s (i = %d): %lf\n", class_names[i].c_str(), i, cyclomatic_complexity_ratio);
            fprintf(stderr, "Red value of cyclomatic complexity shade of function %s (i = %d): %d\n", class_names[i].c_str(), i, GRADIENT_SHADE_RED(cyclomatic_complexity_ratio));
            fprintf(stderr, "Green value of cyclomatic complexity shade of function %s (i = %d): %d\n", class_names[i].c_str(), i, GRADIENT_SHADE_GREEN(cyclomatic_complexity_ratio));
            fprintf(stderr, "Blue value of cyclomatic complexity shade of function %s (i = %d): %d\n", class_names[i].c_str(), i, GRADIENT_SHADE_BLUE(cyclomatic_complexity_ratio));
#endif
            printf(
                "        .fn-%s {\n"
                "            background-color: rgb(%d, %d, %d);\n"
                "        }\n",
                class_names[i].c_str(),
                GRADIENT_SHADE_RED(cyclomatic_complexity_ratio),
                GRADIENT_SHADE_GREEN(cyclomatic_complexity_ratio),
                GRADIENT_SHADE_BLUE(cyclomatic_complexity_ratio)
            );
        }
    }
    printf(
        "    </style>\n"
        "</body>\n"
        "\n"
        "</html>"
    );

    return 0;
}
