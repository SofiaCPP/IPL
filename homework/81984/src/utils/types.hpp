#ifndef RUBY_IPL_UTILS_TYPES_HPP_
#define RUBY_IPL_UTILS_TYPES_HPP_

#include <list>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <regex>
#include <fstream>

template <typename T>
using List = std::list<T>;

template <typename T>
using Vector = std::vector<T>;

template <typename T, typename E>
using Map = std::unordered_map<T, E>;

template <typename T>
using Set = std::unordered_set<T>;

using String = std::string;
using Char = char;
using Index = unsigned int;
using CString = const char*;
using Regex = std::regex;
using RegexCStringMatch = std::cmatch;
using FileReader = std::ifstream;
using FileWriter = std::ofstream;
using FileContentIterator = std::istreambuf_iterator<char>;

#endif
