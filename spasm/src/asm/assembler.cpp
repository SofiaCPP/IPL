#include <cassert>

#include "assembler.hpp"
#include "bytecode.hpp"
#include "symbol.hpp"
#include "tokenizer.hpp"

namespace SpasmImpl
{
namespace ASM
{
Assembler::Assembler(Lexer::Tokenizer& tokenizer, Bytecode_Stream& bytecode)
    : _tokenizer(&tokenizer), _bytecode(&bytecode)
{
}

int get_arg_size(const Lexer::Token args[])
{
    int size = 0;
    for (int i = 0; i < 3; ++i)
    {
        switch (args[i].type())
        {
            case Lexer::Token::Integer:
            case Lexer::Token::XInteger:
            {
                if (args[i].value_int() > 0xffffffff)
                {
                    size = std::max(size, 3);
                }
                else if (args[i].value_int() > 0xffff)
                {
                    size = std::max(size, 2);
                }
                else if (args[i].value_int() > 0xff)
                {
                    size = std::max(size, 1);
                }
                else if (args[i].value_int() < -0xffffffffLL)
                {
                    size = std::max(size, 3);
                }
                else if (args[i].value_int() < -0xffff)
                {
                    size = std::max(size, 2);
                }
                else if (args[i].value_int() < -0xff)
                {
                    size = std::max(size, 1);
                }
                break;
            }
            case Lexer::Token::FloatingPoint:
            {
                size = std::max(size, 3);
                break;
            }
            default:
                break;
        }
    }
    return size;
}

void Assembler::assemble()
{
    Lexer::Token token = _tokenizer->next_token();

    while (token.type() != Lexer::Token::EndInput)
    {
        const auto type = token.type();
        if (type >= Lexer::Token::_NotOpCodeBegin)
        {
            switch (token.type())
            {
                case Lexer::Token::Ident:
                    assemble_identifier(token);
                    break;
                case Lexer::Token::Label:
                    token = _tokenizer->next_token();
                    assert(token.type() == Lexer::Token::Ident);
                    _symbols.define(token.value_str(), _bytecode->size());
                    break;
                default:
                    break;
            }
        }
        else
        {
            Lexer::Token args[3];
            if (type >= Lexer::Token::_OneArgBegin)
            {
                args[0] = _tokenizer->next_token();
            }
            if (type >= Lexer::Token::_TwoArgBegin)
            {
                args[1] = _tokenizer->next_token();
            }
            if (type >= Lexer::Token::_ThreeArgBegin)
            {
                args[2] = _tokenizer->next_token();
            }
            const auto size = get_arg_size(args);
            _bytecode->push_opcode((Bytecode_Stream::Opcode_t)((size << 6) |
                                   token.type()));
            const auto arg_size = 1 << size;

            if (args[0].type() != Lexer::Token::NotUsed)
            {
                if (type == Lexer::Token::Call || type == Lexer::Token::Jump)
                {
                    assert(args[0].type() == Lexer::Token::Ident);
                    assemble_identifier(args[0]);
                }
                else
                {
                    assert(args[0].type() == Lexer::Token::Integer);
                    _bytecode->push_integer(args[0].value_int(), arg_size);
                }
            }
            if (args[1].type() != Lexer::Token::NotUsed)
            {
                if (type == Lexer::Token::JumpF || type == Lexer::Token::JumpT)
                {
                    assert(args[1].type() == Lexer::Token::Ident);
                    assemble_identifier(args[1]);
                }
                else
                {
                    if (args[1].type() == Lexer::Token::Integer)
                    {
                        _bytecode->push_integer(args[1].value_int(), arg_size);
                    }
                    else if (args[1].type() == Lexer::Token::FloatingPoint)
                    {
                        _bytecode->push_double(args[1].value_double());
                    }
                    else
                    {
                        assert(false && "expected a number type");
                    }
                }
            }
            if (args[2].type() != Lexer::Token::NotUsed)
            {
                assert(args[2].type() == Lexer::Token::Integer);
                _bytecode->push_integer(args[2].value_int(), arg_size);
            }
        }

        token = _tokenizer->next_token();
    }

    Symbol_Table::const_iterator i = _symbols.begin();
    while (i != _symbols.end())
    {
        backpatch(i->second);
        ++i;
    }
}

void Assembler::backpatch(const Symbol* symbol)
{
    Symbol::Positions_t::const_iterator i = symbol->positions_begin();
    while (i != symbol->positions_end())
    {
        _bytecode->set_location(*i, symbol->definition());
        ++i;
    }
}

void Assembler::assemble_identifier(const Lexer::Token& token)
{
    Symbol* symbol = _symbols.insert(token.value_str(), _bytecode->size());
    _bytecode->push_location(symbol->definition());
}

bool compile(std::istream& istr, Bytecode_Stream& bytecode)
{
    Lexer::Tokenizer tokenizer(istr);
    Assembler assembler(tokenizer, bytecode);

    assembler.assemble();

    return true;
}

}  // namespace ASM
}  // namespace SpasmImpl
