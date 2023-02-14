#ifndef RUBY_JSIMPL_INCLUDE
#define RUBY_JSIMPL_INCLUDE

#include <iostream>
#define LOG(log)  do {                             \
                    std::cout << log << std::endl; \
                  } while (false);

#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <fstream>
#include <cassert>

using string = std::string;
using exception = std::exception;
using regex = std::regex;
using regex_match = std::cmatch;
using input_file = std::ifstream;
using output_file = std::ofstream;
using char_iterator = std::istreambuf_iterator<char>;

template <typename T>
using vector = std::vector<T>;

template <typename K, typename V>
using map = std::unordered_map<K, V>;

template <typename T>
using ptr = std::shared_ptr<T>;

template <typename T, class... ARGS>
inline ptr<T> create_pointer(ARGS&&... args)
{
  return std::make_shared<T>(args...);
}

inline string read_file(string name)
{
  input_file file(name);
  return string(char_iterator(file), char_iterator());
}

inline void write_file(string name, string content)
{
  output_file file(name);
  file << content;
}

#endif
