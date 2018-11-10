//
// Created by bmestanov on 25.10.18.
//

#ifndef ES2HTML_HIGHLIGHTER_H
#define ES2HTML_HIGHLIGHTER_H

#include "Token.h"

class Highlighter {
public:
    explicit Highlighter(const char *outputPath = nullptr, const char *stylePath = "style.css");

    void highlight(const IPLVector<Token> &tokens);

private:
    const char *stylePath;
    const char *outputPath;

    static IPLString getClass(const Token &token);
};


#endif //ES2HTML_HIGHLIGHTER_H
