#include <memory>
#include <new>
#include <queue>
#include <string>

#include "token.hpp"

namespace SpasmImpl
{
namespace ASM
{
namespace Lexer
{
Token::Token(Token_type type)
    : _type(type), _lineno(0), _value_int(0), _value_str("")
{
}

Token::Token(Token::Token_type type,
             size_t lineno,
             const char* start,
             const char* end)
    : _type(type), _lineno(lineno)
{
    if (_type == Integer || _type == XInteger)
    {
        _value_int = strtol(start, NULL, 0);
    }
    else if (_type == Ident)
    {
        if (end)
        {
            _value_str = std::string(start, end);
        }
        else
        {
            _value_str = std::string(start);
        }
    }
}

Token::Token_type Token::type() const
{
    return _type;
}

int Token::value_int() const
{
    return _value_int;
}

const std::string& Token::value_str() const
{
    return _value_str;
}

size_t Token::lineno() const
{
    return _lineno;
}

}  // namespace Lexer

}  // namespace ASM
}  // namespace SpasmImpl
