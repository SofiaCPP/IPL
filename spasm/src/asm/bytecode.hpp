#ifndef BYTECODE_HPP
#define BYTECODE_HPP

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

namespace SpasmImpl
{
namespace ASM
{
class Bytecode_Stream
{
   public:
    typedef unsigned char byte;
	typedef byte Opcode_t;

    virtual ~Bytecode_Stream();
    virtual void push_opcode(Opcode_t) = 0;
    virtual void push_integer(int) = 0;
    virtual void push_location(size_t) = 0;
    virtual void set_location(size_t, size_t) = 0;
    virtual size_t size() const = 0;
};  // class Bytecode_Stream

class Bytecode_File : public Bytecode_Stream
{
   public:
    Bytecode_File(const std::string&);

    virtual void push_opcode(Opcode_t);
    virtual void push_integer(int);

   private:
    Bytecode_File(const Bytecode_File&);
    Bytecode_File& operator=(const Bytecode_File&);
    void push_byte(Bytecode_Stream::byte);

    std::ofstream _bytecode;

};  // class Bytecode_File

class Bytecode_Memory : public Bytecode_Stream
{
   public:
    virtual void push_opcode(Opcode_t);
    virtual void push_integer(int);
    virtual void push_location(size_t);
    virtual void set_location(size_t, size_t);
    virtual size_t size() const;

    typedef std::vector<Bytecode_Stream::byte> Bytecode;
    const Bytecode& bytecode() const;

   private:
    void push_byte(Bytecode_Stream::byte);

    std::vector<Bytecode_Stream::byte> _bytecode;
};  // class Bytecode_Memory
}  // namespace ASM
}  // namespace SpasmImpl

#endif  // BYTECODE_HPP
