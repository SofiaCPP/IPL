#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "bytecode.hpp"

namespace SpasmImpl
{
namespace ASM
{
Bytecode_Stream::~Bytecode_Stream() {}

Bytecode_File::Bytecode_File(const std::string& filename)
    : _bytecode(filename.c_str(), std::ios_base::out | std::ios_base::binary)
{
}

void Bytecode_File::push_byte(Bytecode_Stream::byte byte)
{
    _bytecode.put(byte);
}

void Bytecode_File::push_opcode(Bytecode_Stream::Opcode_t opcode)
{
    push_byte(opcode);
}

void Bytecode_File::push_integer(int number, int size)
{
    for (int i = 0; i < size; ++i)
        push_byte((number >> (i << 3)) & 0xff);
}

void Bytecode_File::push_double(double number)
{
    auto bits = *reinterpret_cast<size_t*>(&number);
    for (size_t i = 0; i < sizeof(size_t); ++i)
        push_byte((bits >> (i << 3)) & 0xff);
}

void Bytecode_Memory::push_byte(Bytecode_Stream::byte byte)
{
    _bytecode.push_back(byte);
}

void Bytecode_Memory::push_opcode(Bytecode_Stream::Opcode_t opcode)
{
    push_byte(opcode);
}

void Bytecode_Memory::push_integer(int number, int size)
{
    for (int i = 0; i < size; ++i)
        push_byte((number >> (i << 3)) & 0xff);
}

void Bytecode_Memory::push_double(double number)
{
    auto bits = *reinterpret_cast<size_t*>(&number);
    for (size_t i = 0; i < sizeof(size_t); ++i)
        push_byte((bits >> (i << 3)) & 0xff);
}

void Bytecode_Memory::set_location(size_t index, size_t location)
{
    if (location <= 0xff)
    {
        _bytecode[index] = (byte)location;
    }
    else
    {
        for (size_t i = 0; i < sizeof(size_t); ++i)
            _bytecode.at(index + i) = ((location >> (i << 3)) & 0xff);
    }
}

void Bytecode_Memory::push_location(size_t location)
{
    if (location <= 0xff)
    {
        push_byte((byte)location);
    }
    else
    {
        for (size_t i = 0; i < sizeof(size_t); ++i)
            _bytecode.push_back((location >> (i << 3)) & 0xff);
    }
}

const Bytecode_Memory::Bytecode& Bytecode_Memory::bytecode() const
{
    return _bytecode;
}

size_t Bytecode_Memory::size() const
{
    return _bytecode.size();
}

}  // namespace ASM
}  // namespace SpasmImpl
