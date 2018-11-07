#ifndef LEXER_HPP
#define LEXER_HPP

#include <iostream>

#include "token.hpp"

namespace SpasmImpl
{
// ! Namespace for all classes and functions dealing with assembler
namespace ASM
{
//! Namespace for all lexical analisys classes
namespace Lexer
{
/*!
** Class for lexers for the assembler-like language for spasm
*/
class Lexer
{
   public:
    Lexer(std::istream&, size_t = 4096);
    ~Lexer();
    bool tokenize(TokenStream&);

    //! fills the buffer called when the lexer needs more data
    void read(size_t = 1);
    void buffer_init();

   private:
    Lexer(const Lexer&);
    Lexer& operator=(const Lexer&);

    void buffer_grow(size_t);

    //! input stream for the lexer
    std::istream* file;

    //! buffer for the lexer
    char* buffer;

    //! size of the buffer
    size_t buffer_size;

    //! cursor for re2c generated lexer
    char* cursor;

    //! limit for re2c generated lexer
    char* limit;

    //! marker for re2c generated lexer
    char* marker;

    //! pointer to the start of the just recognized token
    char* token_start;

    //! saves the state of the lexer
    unsigned int state;

    //! number of current line
    size_t lineno;
};
}  // namespace Lexer
}  // namespace ASM
}  // namespace SpasmImpl

#endif
