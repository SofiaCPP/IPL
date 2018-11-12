//
// Created by bmestanov on 25.10.18.
//

#include "Highlighter.h"
#include <fstream>
#include <sstream>
#include <iostream>

Highlighter::Highlighter(const char *outputPath, const char *stylePath) :
        outputPath(outputPath), stylePath(stylePath) {}

void Highlighter::highlight(const IPLVector<Token> &tokens) {
    std::stringstream ss;

    ss << "<!doctype html>\n";
    ss << "<html>\n";
    ss << " <head>\n";
    ss << "     <title>hello.js</title>\n";
    ss << "     <link rel=\"stylesheet\" href=\"" << stylePath << "\">\n";
    ss << " </head>\n";
    ss << " <body>\n";
    ss << "     <pre class=\"code\">\n";

    for (auto &token : tokens) {
        IPLString cssClass = getClass(token);
        if (cssClass.empty()) {
            // Print with no formatting
            ss << token.lexeme;
        } else {
            ss << "<span class=\"" << cssClass << "\">" << token.lexeme << "</span>";
        }
    }

    ss << " </body>\n";
    ss << "</html>\n";

    std::ofstream fs(outputPath);
    if (fs) {
        fs << ss.rdbuf();
    } else {
        std::cout << ss.rdbuf();
    }
}

IPLString Highlighter::getClass(const Token &token) {
    switch (token.type) {
        case Whitespace:
        case NewLine:
        case Raw:
            return "";
        case Comment:
            return "comment";
        case Keyword:
            return "keyword";
        case NumericLiteral:
            return "numeric";
        case StringLiteral:
            return "string";
        case TemplateLiteral:
            return "template";
        case Regex:
            return "regex";
        case Identifier:
            return "identifier";
        case BooleanLiteral:
            return "boolean";
        case Global:
            return "global";
    }
}
