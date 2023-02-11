#ifndef RUBY_JSIMPL_INCLUDE
#define RUBY_JSIMPL_INCLUDE

#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <cassert>

using string = std::string;
using exception = std::exception;
using regex = std::regex;
using regex_match = std::cmatch;

template <typename T>
using vector = std::vector<T>;

template <typename K, typename V>
using map = std::unordered_map<K, V>;

template <typename T>
using ptr = std::shared_ptr<T>;

template <typename T, class... ARGS>
inline std::shared_ptr<T> create_pointer(ARGS&&... args)
{
  return std::make_shared<T, ARGS...>(args...);
}

#endif
