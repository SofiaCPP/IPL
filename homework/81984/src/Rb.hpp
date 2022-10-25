#ifndef __RB_HPP__
#define __RB_HPP__

#include <string>
#include <regex>
#include <unordered_map>
#include <vector>
#include <fstream>

namespace Rb {
  typedef unsigned int Index;
  typedef const char*  CString;
  typedef const char*& CStringReference;

  typedef ::std::string String;

  typedef ::std::regex Regex;
  typedef ::std::cmatch RegexMatch;

  typedef ::std::ifstream FileReader;
  typedef ::std::ofstream FileWriter;
  typedef ::std::istreambuf_iterator<char> FileContentIterator;
}

#endif
