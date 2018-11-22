#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <string>

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
        _NoArgBegin,
        Halt = _NoArgBegin,
        Dup,
        Pop,
        _OneArgBegin,
        PopTo = _OneArgBegin,
        PushFrom,
        Push,
        Print,
        Read,
        Call,
        Ret,
        Jump,
        _TwoArgBegin,
        JumpT = _TwoArgBegin,
        JumpF,
        Const,
        _ThreeArgBegin,
        Add = _ThreeArgBegin,
        Sub,
        Mul,
        Div,
        Mod,
        Less,
        LessEq,
        _NotOpCodeBegin,
        Label = _NotOpCodeBegin,
        Ident,
        // Register,
        Integer,
        XInteger,
        FloatingPoint,
        EndInput,
        NotUsed
    };

    Token(Token_type = NotUsed);

    Token(Token_type, size_t, const char* = NULL, const char* = NULL);

    Token_type type() const;

    size_t lineno() const;

    int64_t value_int() const;
    void set_int(int64_t v) { _value_int = v; }

    double value_double() const { return _value_double; }

    const std::string& value_str() const;

   private:
    Token_type _type;

    size_t _lineno;

    int64_t _value_int;
    double _value_double;

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
