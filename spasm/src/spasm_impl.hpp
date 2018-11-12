#ifndef SPASM_IMPL_HPP
#define SPASM_IMPL_HPP

#include <iostream>

#include "frames.hpp"
#include "types.hpp"

namespace SpasmImpl
{
enum OpCodes : char
{
    Halt,
    Dup,
    Pop,
    PopTo,
    Push,
    Print,
    Read,
    Call,
    Ret,
    Jump,
    JumpT,
    JumpF,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessEq,
};
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
    ~Spasm();
    Spasm(const Spasm&) = delete;
    Spasm& operator=(const Spasm&) = delete;

    enum RunResult {
        Success,
        Exception,
        NotImplemented,
    };
    RunResult run();

   private:
    //! Program counter - points the current opcode
    PC_t m_PC = 0;

    typedef SPVector<byte> ByteCode;
    //! bytecode of the program
    ByteCode m_ByteCode;

    typedef SPVector<data_t> DataStack;
    //! stack for storing arguments and local variables
    DataStack data_stack;

    //! Stack pointer - always the top of the stack
    data_t* m_SP = nullptr;

    //! Frame pointer - the start of the stack for the current function
    data_t* m_FP = nullptr;

    //! Return stack for the addresses of the returns
    Rstack_t return_stack;

    //! Stack for the frame of the functions
    FrameStack frame;

    //! Input stream for read () operation
    std::istream* istr;

    //! Output stream for print () opertion
    std::ostream* ostr;

    void push(reg_t reg);
    void pop(reg_t reg);
    void pop();
    void dup();

    void print(reg_t reg);
    void read(reg_t reg);

    void plus(reg_t a0, reg_t a1, reg_t a2);
    void minus(reg_t a0, reg_t a1, reg_t a2);
    void multiply(reg_t a0, reg_t a1, reg_t a2);
    void divide(reg_t a0, reg_t a1, reg_t a2);
    void modulus(reg_t a0, reg_t a1, reg_t a2);

    void gotrue(reg_t a0, reg_t a1);
    void gofalse(reg_t a0, reg_t a1);
    void go(reg_t a0);

    void call();
    void ret(reg_t a0);

    void load();
    void store();

    void less(reg_t a0, reg_t a1, reg_t a2);
    void lesseq(reg_t a0, reg_t a1, reg_t a2);

    data_t get_local(reg_t reg);
    void set_local(reg_t reg, data_t data);
    data_t pop_data();
    void push_data(data_t);
    reg_t read_reg(int size);
};

}  // namespace SpasmImpl
#endif  // #ifndef SPASM_IMPL_HPP
