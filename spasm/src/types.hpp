#ifndef TYPES_HPP
#define TYPES_HPP

#include <cstdlib>
#include <stack>
#include <vector>

#include "value.hpp"

namespace SpasmImpl
{
typedef int8_t byte;
typedef size_t PC_t;
typedef Spasm::Value data_t;

class Spasm;
typedef void (Spasm::*Operation)();

template <typename T>
using SPVector = std::vector<T>;

typedef long long int reg_t;
}  // namespace SpasmImpl
#endif  // ----- #ifndef TYPES_HPP
