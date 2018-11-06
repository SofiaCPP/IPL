#include <iostream>
#include <memory>
#include <new>

#include "lexer.hpp"
#include "token.hpp"
#include "tokenizer.hpp"

namespace SpasmImpl
{
namespace ASM
{
namespace Lexer
{
Tokenizer::Tokenizer(std::istream& istr)
    : _lexer(new Lexer(istr, 1024)), end_input(false)
{
    _lexer->buffer_init();
}

void Tokenizer::push_token(const Token& token)
{
    _tokens.push(token);
    if (token.type() == Token::endinput)
        end_input = true;
}

Token Tokenizer::next_token()
{
    Token t(Token::endinput);
    if (!_tokens.empty())
    {
        t = _tokens.front();
        _tokens.pop();
    }
    else if (!end_input)
    {
        while (_tokens.empty())
        {
            _lexer->read();
            _lexer->tokenize(*this);
        }
        t = _tokens.front();
        _tokens.pop();
    }
    return t;
}

}  // namespace Lexer
}  // namespace ASM
}  // namespace SpasmImpl
