#include <fstream>
#include <iostream>
#include "assembler.hpp"

int main(int argc, const char* argv[])
{
    if (argc != 3)
        return 1;
    SpasmImpl::ASM::Bytecode_Memory bytecode;
    std::ifstream input(argv[1]);
    SpasmImpl::ASM::compile(input, bytecode);

    std::ofstream output(argv[2], std::ios_base::out | std::ios_base::binary);

    // output.write ("SPASM", 5);
    auto& bc = bytecode.bytecode();
    auto size = bc.size();
    output.write(reinterpret_cast<const char*>(&size), sizeof(size));
    output.write(reinterpret_cast<const char*>(bc.data()), size);

    return 0;
}
