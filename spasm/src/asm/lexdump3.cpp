#include <iomanip>
#include <iostream>

#include <cassert>

#include "token.hpp"
#include "tokenizer.hpp"

using namespace SpasmImpl::ASM::Lexer;

#define TOKENS    \
    TOK(push)     \
    TOK(pop)      \
    TOK(dup)      \
    TOK(print)    \
    TOK(read)     \
    TOK(plus)     \
    TOK(minus)    \
    TOK(multiply) \
    TOK(divide)   \
    TOK(modulus)  \
    TOK(gotrue)   \
    TOK(gofalse)  \
    TOK(go)       \
    TOK(call)     \
    TOK(ret)      \
    TOK(load)     \
    TOK(store)    \
    TOK(label)    \
    TOK(ident)    \
    TOK(integer)  \
    TOK(xinteger) \
    TOK(endinput)

const char* token_name[24] = {"halt",
#define TOK(x) #x,
                              TOKENS
#undef TOK
                              "notused!!!"};

int main()
{
    Tokenizer td(std::cin);
    Token token;

    token = td.next_token();
    while (token.type() != Token::EndInput)
    {
        token = td.next_token();
        Token::Token_type ttype = token.type();
        std::cout.width(8);
        std::cout << token.lineno();
        std::cout << ": " << token_name[ttype];
        if (ttype == Token::Integer || ttype == Token::XInteger)
            std::cout << '|' << token.value_int() << '|';
        else if (ttype == Token::Ident)
            std::cout << '|' << token.value_str() << '|';
        std::cout << std::endl;
    }

    return 0;
}
