#include "dstack.hpp"

#include <algorithm>

namespace SpasmImpl
{
Dstack::Dstack() : bottom(NULL), tos(NULL), s_size(0) {}

Dstack::Dstack(const Dstack& ds) : s_size(ds.s_size)
{
    copy_dstack(ds);
}

Dstack& Dstack::operator=(const Dstack& ds)
{
    if (this != &ds)
    {
        if (bottom)
            delete[] bottom;
        s_size = ds.s_size;
        copy_dstack(ds);
    }
    return *this;
}

Dstack::~Dstack()
{
    if (bottom)
        delete[] bottom;
}

/**
 * Makes the stack occupy new_size bytes
 * \param new_size new size for the stack
 */
void Dstack::reserve(size_t new_size)
{
    size_t offset = tos - bottom;
    byte* new_bottom = new byte[new_size];
    if (bottom)
    {
        std::copy(bottom, bottom + std::min(s_size, new_size), new_bottom);
        delete[] bottom;
    }
    bottom = new_bottom;
    tos = new_bottom + offset;
    s_size = new_size;
}

/*!
** Pushes x in the stack.
** If the stack is full its size is doubled.
** \param x data_t object to be pushed on the stack
*/
void Dstack::push(data_t x)
{
    if (bottom + s_size < tos + sizeof(data_t))
        reserve((s_size ? s_size : sizeof(data_t)) * 2);
    *((data_t*)tos) = x;
    tos += sizeof(data_t);
}

/*!
** Pushes the object of size x_size bytes pointed by x in the stack.
** If the stack is full its size is doubled (or more is x_size is greater
** than the size of the stack).
** \param x pointer to the element to be pushed on the stack
** \param x_size size of the element
*/
void Dstack::push(byte* x, size_t x_size)
{
    if (bottom + s_size < tos + x_size)
        reserve(s_size + ((s_size > x_size) ? s_size : x_size));
    std::copy(x, x + x_size, tos);
    tos += x_size;
}

/*!
** Pops an object of size x_size from the stack.
** The object is written to the place pointed by x.
**
** \param x pointer to the place for popping
** \param x_size size of the object to be popped
*/
void Dstack::pop(void* x, size_t x_size)
{
    if (tos - x_size >= bottom)
    {
        std::copy(tos - x_size, tos, (byte*)x);
        tos -= x_size;
    }
}

/*!
** \return a single data_t object, popping it from the stack.
*/
data_t Dstack::pop()
{
    data_t x{};
    if (tos - sizeof(data_t) >= bottom)
    {
        tos -= sizeof(data_t);
        x = *((data_t*)tos);
    }
    return x;
}

/*!
** \return copy of the data_t object that is on top of the stack
*/
data_t Dstack::top() const
{
    data_t x{};
    if (tos - sizeof(data_t) >= bottom)
    {
        x = *((data_t*)(tos - sizeof(data_t)));
    }
    return x;
}

/*!
** \return the size of the stack in bytes
*/
size_t Dstack::size() const
{
    return tos - bottom;
}

/*!
** \return the space reserved for the stack in bytes
*/
size_t Dstack::reserved() const
{
    return s_size;
}

/*!
** Frees unused memory. Note that it is not always possible because memory
** management is done with new / delete
*/

void Dstack::strip_memory()
{
    reserve(size());
}

/*!
** \return true if the stack is empty
*/
bool Dstack::empty() const
{
    return tos - bottom == 0;
}

/*!
** Creates a copy of ds
**
** \param ds data stack to be copied
*/
void Dstack::copy_dstack(const Dstack& ds)
{
    if (ds.bottom)
    {
        bottom = new byte[ds.s_size];
        tos = bottom + (ds.tos - ds.bottom);
        std::copy(ds.bottom, ds.tos, bottom);
    }
    else
        bottom = tos = NULL;
}

}  // namespace SpasmImpl
