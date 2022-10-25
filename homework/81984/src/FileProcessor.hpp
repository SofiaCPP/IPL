#ifndef __FILE_PROCESSOR_HPP__

#include "Rb.hpp"

namespace Rb {
  class FileProcessor {
  public:
    static void WriteFile(String FileName, String Data);
    static String ReadFile(String FileName);
  };
}

#endif
