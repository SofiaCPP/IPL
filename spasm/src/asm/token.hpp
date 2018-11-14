#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <string>

#define TOKENS    \
	TOK(Halt)	  \
    TOK(Dup)      \
    TOK(Pop)      \
    TOK(PopTo)    \
    TOK(Push)     \
    TOK(Alloc)    \
    TOK(Print)    \
    TOK(Read)     \
    TOK(Call)     \
    TOK(Ret)      \
    TOK(Jump)     \
    TOK(JumpT)    \
    TOK(JumpF)    \
    TOK(Const)    \
    TOK(Add)      \
    TOK(Sub)      \
    TOK(Mul)      \
    TOK(Div)      \
    TOK(Mod)      \
    TOK(Less)     \
    TOK(LessEq)   \
    TOK(Label)    \
    TOK(Ident)    \
    TOK(Integer)  \
    TOK(XInteger) \
    TOK(EndInput)

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
		Push,
		Alloc,
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
		//Register,
		Integer,
		XInteger,
		EndInput,
        NotUsed
    };

    Token(Token_type = NotUsed);

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
