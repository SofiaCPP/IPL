#include <iomanip>
#include <iostream>

#include <cassert>

#include "lexer.hpp"
#include "token.hpp"

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

class TokenDumper : public TokenStream
{
   public:
    TokenDumper() : end_reached(false) {}
    void push_token(const Token& token)
    {
        Token::Token_type ttype = token.type();
        std::cout.width(8);
        std::cout << token.lineno();
        std::cout << ": " << token_name[ttype];
        if (ttype == Token::integer || ttype == Token::xinteger)
            std::cout << '|' << token.value_int() << '|';
        else if (ttype == Token::ident)
            std::cout << '|' << token.value_str() << '|';
        std::cout << std::endl;
        if (token.type() == Token::endinput)
            end_reached = true;
    }

    bool input_end() const { return end_reached; }

   private:
    bool end_reached;

    static const char* token_name[24];
};

const char* TokenDumper::token_name[24] = {"halt",
#define TOK(x) #x,
                                           TOKENS
#undef TOK
                                           "notused!!!"};

int main()
{
    Lexer lex(std::cin);
    TokenDumper td;
    Token token;

    lex.buffer_init();

    while (!td.input_end())
    {
        lex.read(1);
        lex.tokenize(td);
    }

    return 0;
}
