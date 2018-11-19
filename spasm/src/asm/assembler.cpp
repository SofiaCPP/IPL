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
            _bytecode->push_opcode((Bytecode_Stream::Opcode_t)token.type());
            if (type >= Lexer::Token::_OneArgBegin)
            {
                if (type == Lexer::Token::Call || type == Lexer::Token::Jump)
                {
                    token = _tokenizer->next_token();
                    assert(token.type() == Lexer::Token::Ident);
                    assemble_identifier(token);
                }
                else
                {
                    token = _tokenizer->next_token();
                    assert(token.type() == Lexer::Token::Integer);
                    _bytecode->push_integer(token.value_int());
                }
            }
            if (type >= Lexer::Token::_TwoArgBegin)
            {
                if (type == Lexer::Token::JumpF || type == Lexer::Token::JumpT)
                {
                    token = _tokenizer->next_token();
                    assert(token.type() == Lexer::Token::Ident);
                    assemble_identifier(token);
                }
                else
                {
                    token = _tokenizer->next_token();
                    assert(token.type() == Lexer::Token::Integer);
                    _bytecode->push_integer(token.value_int());
                }
            }
            if (type >= Lexer::Token::_ThreeArgBegin)
            {
                token = _tokenizer->next_token();
                assert(token.type() == Lexer::Token::Integer);
                _bytecode->push_integer(token.value_int());
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

bool compile(std::istream& istr, Bytecode_Memory& bytecode)
{
    Lexer::Tokenizer tokenizer(istr);
    Assembler assembler(tokenizer, bytecode);

    assembler.assemble();

    return true;
}

}  // namespace ASM
}  // namespace SpasmImpl
