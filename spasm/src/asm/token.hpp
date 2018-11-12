#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <string>

#define TOKENS    \
    TOK(dup)      \
    TOK(push)     \
    TOK(pop)      \
    TOK(print)    \
    TOK(read)     \
    TOK(plus)     \
    TOK(minus)    \
    TOK(multiply) \
    TOK(divide)   \
    TOK(modulus)  \
    TOK(jmpt)     \
    TOK(jmpf)     \
    TOK(jmp)      \
    TOK(call)     \
    TOK(ret)      \
    TOK(load)     \
    TOK(store)    \
    TOK(less)     \
    TOK(lesseq)   \
    TOK(label)    \
    TOK(ident)    \
    TOK(integer)  \
    TOK(xinteger) \
    TOK(endinput)

namespace SpasmImpl
{
namespace ASM
{
namespace Lexer
{
class Token
{
   public:
    enum Token_type
    {
        halt = 0,
#define TOK(x) x,
        TOKENS
#undef TOK
            notused
    };

    Token(Token_type = notused);

    Token(Token_type, size_t, const char* = NULL, const char* = NULL);

    Token_type type() const;

    size_t lineno() const;

    int value_int() const;

    const std::string& value_str() const;

   private:
    Token_type _type;

    size_t _lineno;

    int _value_int;

    std::string _value_str;
};

class TokenStream
{
   public:
    virtual void push_token(const Token&) = 0;
    virtual ~TokenStream(){};
};

}  // namespace Lexer
}  // namespace ASM
}  // namespace SpasmImpl

#undef TOKENS

#endif
