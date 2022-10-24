//
// Created by Marty Kostov on 22.10.22.
//

#ifndef GO2HTML_HTMLBUILDER_HPP
#define GO2HTML_HTMLBUILDER_HPP


#include <vector>
#include <string>

#include "Token.hpp"

class HTMLBuilder {
    unsigned line;
    unsigned column;

    const std::vector<Token> * tokens;

public:
    HTMLBuilder(const std::vector<Token> *tokens);

    std::string getText();

private:
    std::string getWord(const Token &token);
};


#endif //GO2HTML_HTMLBUILDER_HPP
