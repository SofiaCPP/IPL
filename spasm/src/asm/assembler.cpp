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

    while (token.type() != Lexer::Token::endinput)
    {
        switch (token.type())
        {
            case Lexer::Token::ident:
                assemble_identifier(token);
                break;
            case Lexer::Token::label:
                token = _tokenizer->next_token();
                assert(token.type() == Lexer::Token::ident);
                _symbols.define(token.value_str(), _bytecode->size());
                break;
            case Lexer::Token::integer:
            case Lexer::Token::xinteger:
                _bytecode->push_integer(token.value_int());
                break;
            default:
                _bytecode->push_opcode((Bytecode_Stream::Opcode_t)token.type());
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
