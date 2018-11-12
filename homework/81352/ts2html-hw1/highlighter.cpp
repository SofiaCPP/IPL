#include <sstream>
#include <iostream> 
#include <unordered_set>
#include "highlighter.h"

Highlighter::Highlighter(std::vector<Token> tokens): tokens(tokens){}

std::string Highlighter::highlight() {
    std::ostringstream result;
    result << "<!doctype><html><head>";
    result << "<style>.keyword {color:red;} \
                      .operator {color:#0e7c31; font-weight: bold} \
                      .number {color:#d119b2;} .string {color:blue;} \
                      .comment {color:grey; font-style: italic;} \
                      .line_number {opacity: 0.5;} \
                      .decorator {color:#1a1f63; font-style: italic;} \
                      .user_defined_type {color:#105109} \
               </style></head><body><pre>";
    if (!this->tokens.empty()) {
        result << "<span class=\"line_number\">1. </span>";
    }
    int line_number = 2;
    for (Token token: this->tokens) { 
        if (token.getTokenType() == TokenType::NEWLINE) {
            result << "\n<span class=\"line_number\">" << line_number++ << ". </span>";
        } else {
            result << "<span class=\"" << token.getTypeName() << "\">" << token.getTextValue() << "</span>";
        }
    }
    result << "</pre></body></html>";
    return result.str();
}
