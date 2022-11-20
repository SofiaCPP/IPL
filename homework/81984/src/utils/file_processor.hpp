#ifndef RUBY_IPL_UTILS_FILE_PROCESSOR_HPP_
#define RUBY_IPL_UTILS_FILE_PROCESSOR_HPP_

#include "types.hpp"

class FileProcessor
{
public:
  static void WriteFile(String file_name, String data);
  static String ReadFile(String file_name);
};

#endif
