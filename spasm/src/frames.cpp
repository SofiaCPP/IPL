#include <algorithm>

#include "types.hpp"

#include "frames.hpp"

namespace SpasmImpl
{
FrameStack::FrameStack() : base_ptr(NULL), frame_ptr(NULL), g_size(0), c_size(0)
{
}

FrameStack::FrameStack(const FrameStack& fs)
    : g_size(fs.g_size), c_size(fs.c_size), fstack(fs.fstack)
{
    copy_fstack(fs);
}

FrameStack::~FrameStack()
{
    delete_fstack();
}

FrameStack& FrameStack::operator=(const FrameStack& fs)
{
    if (this != &fs)
    {
        delete_fstack();
        copy_fstack(fs);
        g_size = fs.g_size;
        c_size = fs.c_size;
        fstack = fs.fstack;
    }
    return *this;
}

/*!
** Deletes the frame stack
*/
void FrameStack::delete_fstack()
{
    if (base_ptr)
        delete[] base_ptr;
}

/*!
** Copies the frame stack fs
**
** \param fs - the frame stack to be copied
*/
void FrameStack::copy_fstack(const FrameStack& fs)
{
    base_ptr = new data_t[fs.g_size];
    std::copy(fs.base_ptr, fs.frame_ptr + fs.c_size, base_ptr);
    frame_ptr = base_ptr + (fs.frame_ptr - fs.base_ptr);
}

/*!
** Returns a data_t object from the current frame
**
** \param index - index of the data_t object
** \return const reference to the object
*/
const data_t& FrameStack::operator[](size_t index) const
{
    return frame_ptr[index];
}

/*!
** Returns a data_t object from the current frame
**
** \param index - index of the data_t object
** \return reference to the object
*/
data_t& FrameStack::operator[](size_t index)
{
    return frame_ptr[index];
}

/*!
** Removes the current frame from the stack.
** It is called when a function does return.
*/
void FrameStack::pop_frame()
{
    if (fstack.empty())
    {
        frame_ptr = base_ptr;
        c_size = 0;
    }
    else
    {
        data_t* new_frame_ptr = base_ptr + fstack.top();
        fstack.pop();
        c_size = frame_ptr - new_frame_ptr;
        frame_ptr = new_frame_ptr;
    }
}

/*!
** Creates (pushes on the stack) new frame of fsize data_t objects
**
** \param fsize - size in data_t objects of the new frame
*/
void FrameStack::new_frame(size_t fsize)
{
    if (frame_ptr + c_size + fsize > base_ptr + g_size)
    {
        size_t new_size = g_size + std::max(g_size, c_size + fsize);
        data_t* new_base = new data_t[new_size];
        if (base_ptr)
        {
            std::copy(base_ptr, frame_ptr + c_size, new_base);
            delete[] base_ptr;
        }
        frame_ptr = new_base + (frame_ptr - base_ptr);
        base_ptr = new_base;
    }
    fstack.push(frame_ptr - base_ptr);
    c_size = fsize;
}

/*!
** \return true if the stack is empty
*/
bool FrameStack::empty() const
{
    return frame_ptr == base_ptr && c_size == 0;
}
}  // namespace SpasmImpl
