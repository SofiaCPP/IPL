#include <algorithm>
#include <iostream>

#include "spasm.hpp"

namespace SpasmImpl
{
PC_t Spasm::op_size = 21;
// Be sure the same number is on the previous and next lines
Operation Spasm::operations[] = {NULL,
                                 &Spasm::push,
                                 &Spasm::pop,
                                 &Spasm::dup,
                                 &Spasm::print,
                                 &Spasm::read,
                                 &Spasm::plus,
                                 &Spasm::minus,
                                 &Spasm::multiply,
                                 &Spasm::divide,
                                 &Spasm::modulus,
                                 &Spasm::gotrue,
                                 &Spasm::gofalse,
                                 &Spasm::go,
                                 &Spasm::call,
                                 &Spasm::ret,
                                 &Spasm::load,
                                 &Spasm::store,
                                 &Spasm::less,
                                 &Spasm::lesseq};

Spasm::Spasm()
    : pc(0), bc_size(0), bytecode(NULL), istr(&std::cin), ostr(&std::cout)
{
}

/*!
** Constructs new Spasm object
**
** \param _bc_size	- size/length of the bytecode
** \param _bytecode	- the bytecode of the program
** \param _istr		- input stream for the machine
** \param _ostr		- output stream for the machine
*/
Spasm::Spasm(PC_t _bc_size,
             const byte* _bytecode,
             std::istream& _istr,
             std::ostream& _ostr)
    : pc(0), bc_size(_bc_size), istr(&_istr), ostr(&_ostr)
{
    bytecode = new byte[bc_size];
    std::copy(_bytecode, _bytecode + bc_size, bytecode);
}

Spasm::Spasm(const Spasm& m)
    : pc(m.pc),
      bc_size(m.bc_size),
      data_stack(m.data_stack),
      return_stack(m.return_stack),
      frame(m.frame),
      istr(m.istr),
      ostr(m.ostr)
{
    copybc(m);
}

Spasm::~Spasm()
{
    deleteobj();
}

Spasm& Spasm::operator=(const Spasm& m)
{
    if (this != &m)
    {
        deleteobj();
        data_stack = m.data_stack;
        return_stack = m.return_stack;
        frame = m.frame;
        istr = m.istr;
        ostr = m.ostr;
        copybc(m);
    }
    return *this;
}

/*!
** Copies the bytecode of another machine m
**
** \param m	- the machine from the bytecode is copied
*/

void Spasm::copybc(const Spasm& m)
{
    bytecode = new byte[bc_size];
    std::copy(m.bytecode, m.bytecode + bc_size, bytecode);
}

/*!
** Deletes the machine
*/
void Spasm::deleteobj()
{
    if (bytecode)
        delete[] bytecode;
}

/*!
** Returns the dstack, so that it can be examined after execution
**
** \return const reference to the data stack
*/
const Dstack& Spasm::get_dstack() const
{
    return data_stack;
}

/*!
** Starts the machine. The machine stops if it reaches an invalid opcode
** or opcode 0 or the pc reaches beyond the end of the bytecode.
*/
void Spasm::run()
{
    while (pc < bc_size)
    {
        if (bytecode[pc] > 0 && bytecode[pc] < op_size)
            (this->*operations[bytecode[pc]])();
        else
        {
            std::cerr << "opcode got to " << int(bytecode[pc]) << std::endl;
            break;
        }
        ++pc;
    }
}

/*!
** Pushes the next data_t object on the data stack
*/
void Spasm::push()
{
    ++pc;
    data_stack.push(*reinterpret_cast<data_t*>(bytecode + pc));
    pc += sizeof(data_t) - 1;
}

/*!
** Pops a data_t object from the data stack.
*/
void Spasm::pop()
{
    data_stack.pop();
}

/*!
** Dups the data_t object on top of the data stack.
*/
void Spasm::dup()
{
    data_stack.push(data_stack.top());
}

/*!
** Reads a data_t object from the input stream and pushes it on the data
** stack.
*/
void Spasm::read()
{
    data_t x;
    *istr >> x;
    data_stack.push(x);
}

/*!
** Pops a data_t object from the data stack and prints it on the output
** stream.
*/
void Spasm::print()
{
    *ostr << data_stack.pop();
}

/*!
** Pops two data objects from the data stack and pushes their sum on the
** data stack.
*/
void Spasm::plus()
{
    data_t x, y = data_stack.pop();
    x = data_stack.pop();
    data_stack.push(x + y);
}

/*!
** Pops two data objects from the data stack and pushes their difference
** on the data stack.
*/
void Spasm::minus()
{
    data_t x, y = data_stack.pop();
    x = data_stack.pop();
    data_stack.push(x - y);
}

/*!
** Pops two data objects from the data stack and pushes their product
** on the data stack.
*/
void Spasm::multiply()
{
    data_t x, y = data_stack.pop();
    x = data_stack.pop();
    data_stack.push(x * y);
}

/*!
** Pops two data objects from the data stack and pushes their division
** on the data stack.
*/
void Spasm::divide()
{
    data_t x, y = data_stack.pop();
    x = data_stack.pop();
    data_stack.push(x / y);
}

/*!
** Pops two data objects from the data stack and pushes their modulus
** on the data stack.
*/
void Spasm::modulus()
{
    data_t x, y = data_stack.pop();
    x = data_stack.pop();
    data_stack.push(x % y);
}

/*!
** Conditional execution.
** If the data_t object on top of the data stack evaluates to true
** execution continues from the place pointed by number in the next bytes
*/
void Spasm::gotrue()
{
    ++pc;
    if (data_stack.pop())
        pc = *(reinterpret_cast<PC_t*>(bytecode + pc)) - 1;
    else
        pc += sizeof(size_t) - 1;
}

/*!
** Conditional execution.
** If the data_t object on top of the data stack evaluates to false
** execution continues from the place pointed by number in the next bytes
*/
void Spasm::gofalse()
{
    ++pc;
    if (!data_stack.pop())
        pc = *(reinterpret_cast<PC_t*>(bytecode + pc)) - 1;
    else
        pc += sizeof(size_t) - 1;
}

/*!
** Unconditional execution.
** Execution continues from the opcode pointed by the next bytes
*/
void Spasm::go()
{
    ++pc;
    pc = *(reinterpret_cast<PC_t*>(bytecode + pc)) - 1;
}

/*!
** Function call. A new frame with the specified size is created, the
** return address is saved in the return stack and the new pc is loaded.
*/
void Spasm::call()
{
    size_t fs;              // new frame size
    return_stack.push(pc);  // using the ++pc in run()
    data_stack.pop(&fs, sizeof(fs));
    data_stack.pop(&pc, sizeof(pc));
    frame.new_frame(fs);
    --pc;  // the ++pc in run()
}

/*!
** Function return. The frame of the current function is destroyed and the
** saved return address is loaded in the pc
*/
void Spasm::ret()
{
    pc = return_stack.top();
    return_stack.pop();
    frame.pop_frame();
}

/*!
** Local variable with index top of the data stack is pushed on top of the
** data stack.
*/
void Spasm::load()
{
    size_t offset;

    data_stack.pop(&offset, sizeof(offset));
    data_stack.push(frame[offset]);
}

/*!
** Local variable is assigned value
*/
void Spasm::store()
{
    data_t value = data_stack.pop();
    size_t offset;

    data_stack.pop(&offset, sizeof(offset));
    frame[offset] = value;
}

/*!
** Compares the values on top of the stack. Pushes 1 if the value before
** the top of the stack is less than that on top of the stack.
*/
void Spasm::less()
{
    data_t y = data_stack.pop();
    data_t x = data_stack.pop();

    data_stack.push(x < y);
}

/*!
** Compares the values on top of the stack. Pushes 1 if the value before
** the top of the stack is less than or equal to that on top of the stack.
*/
void Spasm::lesseq()
{
    data_t y = data_stack.pop();
    data_t x = data_stack.pop();

    data_stack.push(x <= y);
}

}  // namespace SpasmImpl
