#include "file_processor.hpp"

void FileProcessor::WriteFile(String file_name, String data)
{
  FileWriter File(file_name);
  File << data;
}

String FileProcessor::ReadFile(String file_name)
{
  FileReader file(file_name);
  return String((FileContentIterator(file)), FileContentIterator());
}
