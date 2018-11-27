#ifndef ASSEMBPLER_HPP
#define ASSEMBPLER_HPP

#include <iostream>

#include "bytecode.hpp"
#include "symbol.hpp"
#include "tokenizer.hpp"

namespace SpasmImpl
{
namespace ASM
{
class Assembler
{
   public:
    Assembler(Lexer::Tokenizer&, Bytecode_Stream&);

    void assemble();

   private:
    void backpatch(const Symbol*);
    void assemble_identifier(const Lexer::Token&);

    Lexer::Tokenizer* _tokenizer;
    Bytecode_Stream* _bytecode;

    Symbol_Table _symbols;

};  // class Assembler

bool compile(std::istream&, Bytecode_Stream& bytecode);
}  // namespace ASM
}  // namespace SpasmImpl

#endif  // ASSEMBPLER_HPP
