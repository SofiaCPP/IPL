#include <fstream>
#include <iostream>
#include "assembler.hpp"

int main(int argc, const char* argv[])
{
    if (argc != 2)
        return 1;
    SpasmImpl::ASM::Bytecode_Memory bytecode;
    SpasmImpl::ASM::compile(std::cin, bytecode);

    std::ofstream output(argv[1], std::ios_base::out | std::ios_base::binary);

    // output.write ("SPASM", 5);
    auto& bc = bytecode.bytecode();
    auto size = bc.size();
    output.write(reinterpret_cast<const char*>(&size), sizeof(size));
    output.write(reinterpret_cast<const char*>(bc.data()), size);

    return 0;
}
