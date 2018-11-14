#include <algorithm>
#include <cassert>
#include <iostream>

#include "spasm.hpp"

namespace SpasmImpl
{
Spasm::Spasm() {}

/*!
** Constructs new Spasm object
**
** \param _bc_size	- size/length of the bytecode
** \param _bytecode	- the bytecode of the program
** \param _istr		- input stream for the machine
** \param _ostr		- output stream for the machine
*/
void Spasm::Initialize(PC_t _bc_size,
                       const byte* _bytecode,
                       std::istream& _istr,
                       std::ostream& _ostr)

{
    m_PC = 0;
    m_ByteCode.assign(_bytecode, _bytecode + _bc_size);
    istr = &_istr;
    ostr = &_ostr;
    data_stack.resize(1024);
    m_SP = &data_stack[0];
    m_FP = &data_stack[0];
}

Spasm::~Spasm() {}

/*!
** Runs the machine. The machine stops if it reaches an invalid opcode
** or opcode 0 or the pc reaches beyond the end of the bytecode.
** @return error code for success or failure
*/
Spasm::RunResult Spasm::run()
{
    const auto codeSize = m_ByteCode.size();
    while (m_PC < codeSize)
    {
        const auto instruction = m_ByteCode[m_PC++];
        const auto opcode = OpCodes(instruction & 0x3f);
        const auto size = instruction >> 6;
        assert(size == 0 && "no long args yet");
        switch (opcode)
        {
            case OpCodes::Halt:
                return RunResult::Success;
            case OpCodes::Dup:
                dup();
                break;
            case OpCodes::Pop:
                pop();
                break;
            case OpCodes::PopTo:
                break;
            case OpCodes::Push:
            {
                const auto arg0 = read_reg(size);
                push(arg0);
                break;
            }
            case OpCodes::Alloc:
            {
                const auto count = PC_t(read_reg(size));
                for (PC_t i = 0; i < count; ++i)
                {
                    push_data(0);
                }
                break;
            }
            case OpCodes::Print:
            {
                const auto arg0 = read_reg(size);
                print(arg0);
                break;
            }
            case OpCodes::Read:
            {
                const auto arg0 = read_reg(size);
                read(arg0);
                break;
            }
            case OpCodes::Call:
            {
                const auto arg0 = read_reg(size);
                call(arg0);
                break;
            }
            case OpCodes::Ret:
            {
                const auto arg0 = read_reg(size);
                ret(arg0);
                break;
            }
            case OpCodes::Jump:
            {
                const auto arg0 = read_reg(size);
                go(arg0);
                break;
            }
            case OpCodes::JumpT:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                gotrue(arg0, arg1);
                break;
            }
            case OpCodes::JumpF:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                gofalse(arg0, arg1);
                break;
            }
            case OpCodes::Const:
            {
                const auto reg = read_reg(size);
                const auto value = read_number(size);
                set_local(reg, value);
                break;
            }
            case OpCodes::Add:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                const auto arg2 = read_reg(size);
                plus(arg0, arg1, arg2);
                break;
            }
            case OpCodes::Sub:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                const auto arg2 = read_reg(size);
                minus(arg0, arg1, arg2);
                break;
            }
            case OpCodes::Mul:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                const auto arg2 = read_reg(size);
                multiply(arg0, arg1, arg2);
                break;
            }
            case OpCodes::Div:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                const auto arg2 = read_reg(size);
                divide(arg0, arg1, arg2);
                break;
            }
            case OpCodes::Mod:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                const auto arg2 = read_reg(size);
                modulus(arg0, arg1, arg2);
                break;
            }
            case OpCodes::Less:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                const auto arg2 = read_reg(size);
                less(arg0, arg1, arg2);
                break;
            }
            case OpCodes::LessEq:
            {
                const auto arg0 = read_reg(size);
                const auto arg1 = read_reg(size);
                const auto arg2 = read_reg(size);
                lesseq(arg0, arg1, arg2);
                break;
            }
            default:
            {
                std::cerr << opcode << ": not implemented" << std::endl;
                return RunResult::NotImplemented;
            }
        }
    }
    return RunResult::Success;
}

/*!
** Pushes the next data_t object on the data stack
*/
void Spasm::push(reg_t reg)
{
    push_data(get_local(reg));
}

/*!
** Pops a data_t object from the data stack.
*/
void Spasm::pop()
{
    --m_SP;
}

/*!
** Dups the data_t object on top of the data stack.
*/
void Spasm::dup()
{
    push_data(*(m_SP - 1));
}

/*!
** Reads a data_t object from the input stream and pushes it on the data
** stack.
*/
void Spasm::read(reg_t reg)
{
    data_t x;
    *istr >> x;
    set_local(reg, x);
}

/*!
** Pops a data_t object from the data stack and prints it on the output
** stream.
*/
void Spasm::print(reg_t reg)
{
    *ostr << get_local(reg);
}

/*!
** Pops two data objects from the data stack and pushes their sum on the
** data stack.
*/
void Spasm::plus(reg_t a0, reg_t a1, reg_t a2)
{
    set_local(a0, get_local(a1) + get_local(a2));
}

/*!
** Pops two data objects from the data stack and pushes their difference
** on the data stack.
*/
void Spasm::minus(reg_t a0, reg_t a1, reg_t a2)
{
    set_local(a0, get_local(a1) - get_local(a2));
}

/*!
** Pops two data objects from the data stack and pushes their product
** on the data stack.
*/
void Spasm::multiply(reg_t a0, reg_t a1, reg_t a2)
{
    set_local(a0, get_local(a1) * get_local(a2));
}

/*!
** Pops two data objects from the data stack and pushes their division
** on the data stack.
*/
void Spasm::divide(reg_t a0, reg_t a1, reg_t a2)
{
    set_local(a0, get_local(a1) / get_local(a2));
}

/*!
** Pops two data objects from the data stack and pushes their modulus
** on the data stack.
*/
void Spasm::modulus(reg_t a0, reg_t a1, reg_t a2)
{
    set_local(a0, get_local(a1) % get_local(a2));
}

/*!
** Conditional execution.
** If the data_t object on top of the data stack evaluates to true
** execution continues from the place pointed by number in the next bytes
*/
void Spasm::gotrue(reg_t a0, reg_t a1)
{
    if (get_local(a0))
    {
        m_PC = a1;
    }
}

/*!
** Conditional execution.
** If the data_t object on top of the data stack evaluates to false
** execution continues from the place pointed by number in the next bytes
*/
void Spasm::gofalse(reg_t a0, reg_t a1)
{
    if (!get_local(a0))
    {
        m_PC = a1;
    }
}

/*!
** Unconditional execution.
** Execution continues from the opcode pointed by the next bytes
*/
void Spasm::go(reg_t a0)
{
    m_PC = a0;
}

/*!
** Function call. A new frame with the specified size is created, the
** return address is saved in the return stack and the new pc is loaded.
*/
void Spasm::call(reg_t a0)
{
    Frame call{m_PC, PC_t(m_FP - &data_stack[0]),
               m_SP - &data_stack[0] - PC_t(*(m_SP - 1)) - 1};
    m_Frames.push(std::move(call));
    m_FP = m_SP - 1;
    go(a0);
}

/*!
** Function return. The frame of the current function is destroyed and the
** saved return address is loaded in the pc
*/
void Spasm::ret(reg_t reg)
{
    Frame parent = m_Frames.top();
    m_Frames.pop();
    m_SP = &data_stack[parent.StackPointer];
    *(m_SP - 1) = m_FP[reg];
    m_FP = &data_stack[parent.FramePointer];
    m_PC = parent.ReturnAddress;
}

/*!
** Local variable with index top of the data stack is pushed on top of the
** data stack.
*/
void Spasm::load()
{
    assert(false && "not-implemented");
}

/*!
** Local variable is assigned value
*/
void Spasm::store()
{
    assert(false && "not-implemented");
}

/*!
** Compares the values on top of the stack. Pushes 1 if the value before
** the top of the stack is less than that on top of the stack.
*/
void Spasm::less(reg_t a0, reg_t a1, reg_t a2)
{
    set_local(a0, get_local(a1) < get_local(a2));
}

/*!
** Compares the values on top of the stack. Pushes 1 if the value before
** the top of the stack is less than or equal to that on top of the stack.
*/
void Spasm::lesseq(reg_t a0, reg_t a1, reg_t a2)
{
    set_local(a0, get_local(a1) <= get_local(a2));
}

data_t Spasm::get_local(reg_t reg)
{
    assert(&data_stack[0] <= (m_FP + reg));
    assert((m_FP + reg) < &data_stack[data_stack.size() - 1]);
    return m_FP[reg];
}

void Spasm::set_local(reg_t reg, data_t data)
{
    assert(&data_stack[0] <= (m_FP + reg));
    assert((m_FP + reg) < &data_stack[data_stack.size() - 1]);
    m_FP[reg] = data;
}

data_t Spasm::pop_data()
{
    auto result = data_stack.back();
    data_stack.pop_back();
    return result;
}

void Spasm::push_data(data_t data)
{
    assert(m_SP < &data_stack[data_stack.size() - 1]);
    *(m_SP++) = data;
}

reg_t Spasm::read_reg(size_t size)
{
    assert(size == 0);
    return m_ByteCode[m_PC++];
}

data_t Spasm::read_number(size_t size)
{
    assert(size == 0);
    return m_ByteCode[m_PC++];
}

}  // namespace SpasmImpl
