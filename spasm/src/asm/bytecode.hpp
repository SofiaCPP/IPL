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
    typedef uint8_t byte;
    typedef byte Opcode_t;

    virtual ~Bytecode_Stream();
    virtual void push_opcode(Opcode_t) = 0;
    virtual void push_integer(int64_t, int size) = 0;
    virtual void push_double(double) = 0;
    virtual void push_location(size_t) = 0;
    virtual void push_string(const char* s, size_t length, int size) = 0;
    virtual void set_location(size_t, size_t) = 0;
    virtual size_t size() const = 0;
};  // class Bytecode_Stream

class Bytecode_File : public Bytecode_Stream
{
   public:
    Bytecode_File(const std::string&);

    void push_opcode(Opcode_t) override;
    void push_integer(int64_t, int size) override;
    void push_double(double) override;

   private:
    Bytecode_File(const Bytecode_File&);
    Bytecode_File& operator=(const Bytecode_File&);
    void push_byte(Bytecode_Stream::byte);

    std::ofstream _bytecode;

};  // class Bytecode_File

class Bytecode_Memory : public Bytecode_Stream
{
   public:
    void push_opcode(Opcode_t) override;
    void push_integer(int64_t, int) override;
    void push_double(double) override;
    void push_location(size_t) override;
    void set_location(size_t, size_t) override;
    void push_string(const char* s, size_t length, int size) override;
    size_t size() const override;

    typedef std::vector<Bytecode_Stream::byte> Bytecode;
    const Bytecode& bytecode() const;

   private:
    void push_byte(Bytecode_Stream::byte);

    std::vector<Bytecode_Stream::byte> _bytecode;
};  // class Bytecode_Memory
}  // namespace ASM
}  // namespace SpasmImpl

#endif  // BYTECODE_HPP
