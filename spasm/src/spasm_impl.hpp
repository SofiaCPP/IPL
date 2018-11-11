#ifndef SPASM_IMPL_HPP
#define SPASM_IMPL_HPP

#include <iostream>

#include "frames.hpp"
#include "types.hpp"

namespace SpasmImpl
{
//! The Abstract Stack Machine
/*!
** The machine contains a data stack for operations and control flow and
** frame stack for function calls (including parameter passing) and local
** variables and return stack for the addresses of the returns.
*/
class Spasm
{
   public:
    Spasm();
    Spasm(PC_t,
          const byte*,
          std::istream& = std::cin,
          std::ostream& = std::cout);
    Spasm(const Spasm&);
    ~Spasm();
    Spasm& operator=(const Spasm&);

    void run();

   private:
    //! Operations (opcodes) of the machine
    static Operation operations[];

    //! number of operations
    static PC_t op_size;

    //! Program counter - points the current opcode
    PC_t pc;

    //! size of the program bytecode
    PC_t bc_size;

    //! bytecode of the program
    byte* bytecode;

    typedef SPVector<data_t> DataStack;
    //! Data stack for arithmetic operations and control flow
    DataStack data_stack;

    //! Return stack for the addresses of the returns
    Rstack_t return_stack;

    //! Stack for the frame of the functions
    FrameStack frame;

    //! Input stream for read () operation
    std::istream* istr;

    //! Output stream for print () opertion
    std::ostream* ostr;

    void push();
    void pop();
    void dup();

    void print();
    void read();

    void plus();
    void minus();
    void multiply();
    void divide();
    void modulus();

    void gotrue();
    void gofalse();
    void go();

    void call();
    void ret();

    void load();
    void store();

    void less();
    void lesseq();

    // private method for copying and deleting an object
    void copybc(const Spasm&);
    void deleteobj();

    data_t pop_data();
    void push_data(data_t);
};

}  // namespace SpasmImpl
#endif  // #ifndef SPASM_IMPL_HPP
