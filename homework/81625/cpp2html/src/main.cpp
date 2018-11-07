#include <iostream>
#include <fstream>
#include <vector>
#include "Lexer.h"

void printTokens(std::vector<Token> &tokens)
{
    for(int i = 0;i<tokens.size();i++)
    {
        std::cout<<tokens[i].type<<" "<<tokens[i].lexeme <<" on line " << tokens[i].line <<std::endl;
    }
}

void cpp2html(std::vector<Token> &tokens, const char* code)
{
    std::ofstream out("../cpp2.html");
    std::string header = "<!DOCTYPE html>\n"
                            "<html lang=\"en\">\n"
                            "<head>\n"
                            "  <style>\n"
                            "    body {\n"
                            "      font-size: 20px;\n"
                            "    }\n\n"
                            "    .number {\n"
                            "      color: pink;\n"
                            "    }\n\n"
                            "    .string {\n"
                            "      color: blue;\n"
                            "    }\n\n"
                            "    .keyword {\n"
                            "      color: purple;\n"
                            "      font-weight: bold;\n"
                            "    }\n\n"
                            "    .operator {\n"
                            "      color: red;\n"
                            "    }\n\n"
                            "    .comment {\n"
                            "      color: gray;\n"
                            "      font-style: italic;\n"
                            "    }\n\n"
                            "    .preprocessor {\n"
                            "      color: green;\n"
                            "      font-weight: bold;\n"
                            "    }\n\n"
                            "  </style>\n"
                            "</head>\n"
                            "<body>\n";
    out << header;
    unsigned codeInd = 0;
    for(int i = 0;i < tokens.size();i++)
    {
        while(code[codeInd] == ' ' || code[codeInd] == '\t' || code[codeInd] == '\n')
        {
            if(code[codeInd] == '\n') out << "<br>";
            else out << "&nbsp";
            //else out <<
            ++codeInd;
        }
        if(tokens[i].type == TokenType::NUMBER)
        {
            out << "<span class=\"number\">" << tokens[i].value <<"</span>";
        }
        else if(tokens[i].type == TokenType::STRING)
        {
            out << "<span class=\"string\">" << tokens[i].lexeme <<"</span>";
        }
        else if(tokens[i].type == TokenType::COMMENT)
        {
            out << "<span class=\"comment\">" << tokens[i].lexeme <<"</span>";
        }
        else if(tokens[i].type <= TokenType::RIGHT_SHIFT_EQUAL)
        {
            out << "<span class=\"operator\">" << tokens[i].lexeme <<"</span>";
        }
        else if(tokens[i].type <= TokenType::XOR_EQ)
        {
            out << "<span class=\"keyword\">" << tokens[i].lexeme <<"</span>";
        }
        else if(tokens[i].type <= TokenType::PRE_PRAGMA)
        {
            out << "<span class=\"preprocessor\">" << tokens[i].lexeme <<"</span>";
        }
        else
        {
            out << tokens[i].lexeme;
        }
        codeInd += tokens[i].lexeme.length();
    }
    out << "\n</body>\n</html>";
    out.close();
}

int main()
{
    char code[] = "#include<iostream>\nbool isCapitalCase(char c)\n{\n    return c >= 'A' && c <= 'Z';\n}\nm_KeyWordsTable[\"break\"] = TokenType::Break;\n// this is a comment test\n"
                    "/* another one */";
    Tokenizer tokenizer(code);
    std::vector<Token> tokens;

    if(tokenizer.tokenize(tokens))
    {
        cpp2html(tokens, code);
    }
    //printTokens(tokens);
    return 0;
}

