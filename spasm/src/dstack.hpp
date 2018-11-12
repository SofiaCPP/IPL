#ifndef DSTACK_HPP
#define DSTACK_HPP

#include "types.hpp"

namespace SpasmImpl
{
//! class representing the data stack

/*! The class is quite simple stack. It is byte-orientatied - every push
** and pop is done in bytes with no taking care of any byte order!
*/
class Dstack
{
   public:
    Dstack();
    Dstack(const Dstack&);
    Dstack& operator=(const Dstack&);
    ~Dstack();
    void push(data_t);
    void push(byte*, size_t);
    void pop(void*, size_t);
    data_t pop();
    data_t top() const;

    void reserve(size_t);
    void strip_memory();
    size_t size() const;
    size_t reserved() const;
    bool empty() const;

   private:
    void copy_dstack(const Dstack&);

    //! pointer to the bottom of the stack
    byte* bottom;

    //! pointer to the top of the stack
    byte* tos;

    //! size of the stack in bytes
    size_t s_size;
};
}  // namespace SpasmImpl

#endif  // #ifndef DSTACK_HPP
