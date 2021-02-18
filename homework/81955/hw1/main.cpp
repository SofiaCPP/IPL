#include "Lexer.h"
#include <iostream>

int main()
{
    std::ifstream file("test.go");
    std::string code_str((std::istreambuf_iterator<char>(file)), (std::istreambuf_iterator<char>()));

    Tokenizer tokenizer(code_str.c_str());
    LexerResult result = tokenizer.tokenize();

    GenerateHTML::generate(result, "go2html.html");
    file.close();

    return 0;
}
