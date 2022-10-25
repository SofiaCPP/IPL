#include "FileProcessor.hpp"

namespace Rb {
  void FileProcessor::WriteFile(String FileName, String Data) {
    FileWriter File(FileName);
    File << Data;
  }

  String FileProcessor::ReadFile(String FileName) {
    FileReader File(FileName);
    return String((FileContentIterator(File)), FileContentIterator());
  }
}
