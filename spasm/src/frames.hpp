#ifndef FRAMES_HPP
#define FRAMES_HPP

#include <stack>

#include "types.hpp"

namespace SpasmImpl
{
//! Class representing the stack frame for a function
/*!
** The class is implemented as a simple stack of frames while each frame
** is built up by a number of data_t objects
*/
class FrameStack
{
   public:
    FrameStack();
    FrameStack(const FrameStack&);
    ~FrameStack();
    FrameStack& operator=(const FrameStack&);

    const data_t& operator[](size_t) const;
    data_t& operator[](size_t);
    void pop_frame();
    void new_frame(size_t);
    bool empty() const;

   private:
    //! pointer to the base of the stack
    data_t* base_ptr;

    //! pointer to the beginning of the current frame
    data_t* frame_ptr;

    //! size of the whole stack in data_t objects
    size_t g_size;

    //! size of the current frame in data_t objects
    size_t c_size;

    //! stack of the offsets between frames in the stack.
    std::stack<size_t> fstack;

    void copy_fstack(const FrameStack&);
    void delete_fstack();
};

}  // namespace SpasmImpl

#endif  // #ifndef FRAMES_HPP
