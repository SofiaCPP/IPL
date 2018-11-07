#ifndef TOKENIZER_HPP
#define TOKENIZER_HPP

#include <iostream>
#include <memory>
#include <queue>

#include "lexer.hpp"
#include "token.hpp"

namespace SpasmImpl
{
namespace ASM
{
namespace Lexer
{
class Tokenizer : public TokenStream
{
   public:
    Tokenizer(std::istream&);

    virtual void push_token(const Token&);
    Token next_token();

   private:
    std::unique_ptr<Lexer> _lexer;
    std::queue<Token> _tokens;
    bool end_input;
};  // class Tokenizer
}  // namespace Lexer

}  // namespace ASM
}  // namespace SpasmImpl

#endif
