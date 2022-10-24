#include <iostream>

#include <fstream>

#include "Lexer.hpp"
#include "HTMLBuilder.hpp"

int main() {

    std::string fileName;
    std::cout << "Enter file name: ";
    getline(std::cin, fileName);

    std::ifstream input(fileName);
    input.seekg(0, std::ios::end);
    std::string code;
    int size = (int)input.tellg();
    code.resize(size);
    input.seekg(0, std::ios::beg);
    input.read(&code[0], code.size());
    input.close();

    auto lexer = Lexer(code);
    const std::vector<Token>& tokens = lexer.tokenize();

    auto htmlBuilder = HTMLBuilder(&tokens);

    const auto& htmlText = htmlBuilder.getText();

    std::ofstream output(fileName+".html");
    output << htmlText;

}
