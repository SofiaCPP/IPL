#ifndef HIGHLIGHTER
#define HIGHLIGHTER

#include <vector>
#include <string>
#include "token.h"

class Highlighter {
    private:
        std::vector<Token> tokens;
        void preProcessTokens();
    public:
        Highlighter(std::vector<Token>);
        std::string highlight();

};
#endif 