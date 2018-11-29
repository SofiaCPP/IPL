#include <iostream>

#include "lexer.hpp"

#include "token.hpp"

namespace SpasmImpl
{
namespace ASM
{
//! Namespace for all lexical analisys classes
namespace Lexer
{

Lexer::Lexer (std::istream & _file, size_t _buffer_size)
        : file (&_file), buffer_size (_buffer_size), state (unsigned(-1)),
        lineno (0)
{
        buffer = new char[buffer_size];
        cursor = limit = marker = token_start = NULL;
}

Lexer::~Lexer ()
{
        delete [] buffer;
}

bool Lexer::tokenize (TokenStream &ts)
{
/*!re2c
re2c:indent:top = 1;

re2c:define:YYFILL="return true;";
re2c:define:YYFILL:naked=1;
re2c:define:YYCTYPE="char";
re2c:define:YYCURSOR=cursor;
re2c:define:YYLIMIT=limit;
re2c:define:YYMARKER=marker;
re2c:define:YYGETSTATE=state;
re2c:define:YYGETSTATE:naked=1;
re2c:define:YYSETSTATE="state = ";
re2c:define:YYSETSTATE:naked=1;
*/

/*!getstate:re2c */

                                while (1) {

/*!re2c

DIGIT   = [0-9] ;
XDIGIT  = [0-9a-fA-F] ;

INTEGER         = "-"? "0" | ([1-9] DIGIT*) ;
NONNEGATIVE     = "0" | ([1-9] DIGIT*) ;
XINTEGER        = "0" [xX] XDIGIT+ ;
FLOATING        = "-"? "0" | ([1-9] DIGIT*) "." NONNEGATIVE ;
IDENTIFIER      = [a-zA-Z_] [0-9a-zA-Z_]* ;
SSTRING         = "'" ([^']|"\\'")* "'" ;
DSTRING         = "\"" ([^"]|"\\\"")* "\"" ;

INTEGER         {
                                ts.push_token (Token (Token::Integer, lineno,
                                token_start, cursor));
                                token_start = cursor;

                                continue;
                        }

FLOATING                {
                                ts.push_token (Token (Token::FloatingPoint,
                                                      lineno, token_start,
                                                      cursor));
                                token_start = cursor;

                                continue;
                        }

"r" NONNEGATIVE         {
                                ts.push_token (Token (Token::Integer, lineno,
                                token_start + 1, cursor));
                                token_start = cursor;

                                continue;
                        }
"a" NONNEGATIVE         {
                                Token t(Token::Integer, lineno,
                                                token_start + 1, cursor);
                                t.set_int(-t.value_int());
                                ts.push_token(t);
                                token_start = cursor;
                                continue;
                        }

XINTEGER        {
                                ts.push_token (Token (Token::XInteger, lineno,
                                token_start, cursor));
                                token_start = cursor;

                                continue;
                        }

"pushr"         {
                                ts.push_token (Token (Token::PushFrom, lineno));
                                token_start = cursor;

                                continue;
                        }
"push"          {
                                ts.push_token (Token (Token::Push, lineno));
                                token_start = cursor;

                                continue;
                        }
"pop"           {
                                ts.push_token (Token (Token::Pop, lineno));
                                token_start = cursor;

                                continue;
                        }
"popr"          {
                                ts.push_token (Token (Token::PopTo, lineno));
                                token_start = cursor;

                                continue;
                        }

"dup"           {
                                ts.push_token (Token (Token::Dup, lineno));
                                token_start = cursor;

                                continue;
                        }

"read"          {
                                ts.push_token (Token (Token::Read, lineno));
                                token_start = cursor;

                                continue;
                        }

"print"         {
                                ts.push_token (Token (Token::Print, lineno));
                                token_start = cursor;

                                continue;
                        }

"add"                   {
                                ts.push_token (Token (Token::Add, lineno));
                                token_start = cursor;

                                continue;
                        }

"sub"                   {
                                ts.push_token (Token (Token::Sub, lineno));
                                token_start = cursor;

                                continue;
                        }

"mul"                   {
                                ts.push_token (Token (Token::Mul, lineno));
                                token_start = cursor;

                                continue;
                        }

"div"                   {
                                ts.push_token (Token (Token::Div, lineno));
                                token_start = cursor;

                                continue;
                        }

"mod"                   {
                                ts.push_token (Token (Token::Mod, lineno));
                                token_start = cursor;

                                continue;
                        }

"jmpt"  {
                                ts.push_token (Token (Token::JumpT, lineno));
                                token_start = cursor;

                                continue;
                        }

"jmpf"  {
                                ts.push_token (Token (Token::JumpF, lineno));
                                token_start = cursor;

                                continue;
                        }

"jmp"           {
                                ts.push_token (Token (Token::Jump, lineno));
                                token_start = cursor;

                                continue;
                }
"const"         {
                                ts.push_token (Token (Token::Const, lineno));
                                token_start = cursor;

                                continue;
                }

"string"        {
                                ts.push_token (Token (Token::String, lineno));
                                token_start = cursor;

                                continue;
                }

"call"          {
                                ts.push_token (Token (Token::Call, lineno));
                                token_start = cursor;

                                continue;
                        }

"ret"           {
                                ts.push_token (Token (Token::Ret, lineno));
                                token_start = cursor;

                                continue;
                        }

"leq"           {
                                ts.push_token (Token (Token::LessEq, lineno));
                                token_start = cursor;

                                continue;
                        }
"less"                  {
                                ts.push_token (Token (Token::Less, lineno));
                                token_start = cursor;

                                continue;
                        }

"label"         {
                                ts.push_token (Token (Token::Label, lineno));
                                token_start = cursor;

                                continue;
                        }

IDENTIFIER      {
                                ts.push_token (Token (Token::Ident, lineno, token_start,
                                                        cursor));
                                token_start = cursor;

                                continue;
                        }
SSTRING | DSTRING      {
                                ts.push_token (Token (Token::StringValue, lineno, token_start + 1,
                                                        cursor - 1));
                                token_start = cursor;

                                continue;
                        }

[       ]+              {
                                token_start = cursor;

                                continue;
                        }
DIGIT* ":"         {
                                token_start = cursor;

                                continue;
                        }
"#" .* "\n"				{
                                ++lineno;
                                token_start = cursor;

                                continue;
                        }
"\n"		            {
                                ++lineno;
                                token_start = cursor;

                                continue;
                        }

"\000"          {
                                ts.push_token (Token (Token::EndInput, lineno));

                                break;
                        }

[^]                     {
                                return false;
                        }

*/
                                }
                                return true;
                        }

/*!
** Fills up the buffer when neccessary.
** Note the buffer is always filled to the maximum, not just the
** number of bytes neccessary for the lexer to continue.
**
** \param nbytes        - the number of bytes
*/

void Lexer::buffer_grow (size_t new_size)
{
        char *new_buffer = new char[new_size];

        std::copy (buffer, limit, new_buffer);

        token_start = new_buffer + (token_start - buffer);
        cursor = new_buffer + (cursor - buffer);
        limit = new_buffer + new_size;

        buffer_size = new_size;

        delete [] buffer;
        buffer = new_buffer;
}

void Lexer::read (size_t nbytes)
{
        if (token_start + nbytes > limit) { // we need larger buffer
                buffer_grow (token_start - buffer + nbytes);
        }

        std::copy (token_start, limit, buffer);
        cursor -= token_start - buffer;
        char *old_limit = limit - (token_start - buffer);
        token_start = buffer;

        file->read (old_limit, limit - old_limit);
        if (file->eof ()) {
                size_t bytes = file->gcount ();
                std::fill (old_limit + bytes, limit, '\0');
        }
}

void Lexer::buffer_init ()
{
        file->read (buffer, buffer_size);
        if (file->eof ()) {
                size_t bytes = file->gcount ();
                std::fill (buffer + bytes, buffer + buffer_size, '\0');
        }

        cursor = marker = token_start = buffer;
        limit = buffer + buffer_size;
}


}       // namespace Lexer

}       // namespace ASM
}       // namespace SpasmImpl

