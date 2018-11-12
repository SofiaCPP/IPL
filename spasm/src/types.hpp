#ifndef TYPES_HPP
#define TYPES_HPP

#include <cstdlib>
#include <stack>
#include <vector>

namespace SpasmImpl
{
typedef unsigned char byte;
typedef size_t PC_t;
typedef int data_t;
typedef std::stack<PC_t> Rstack_t;

class Spasm;
typedef void (Spasm::*Operation)();

template <typename T>
using SPVector = std::vector<T>;

typedef long long int reg_t;
}  // namespace SpasmImpl
#endif  // ----- #ifndef TYPES_HPP
