#include "bf_interpreter.h"
#include "CLI11.hpp"

#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>

#define USAGE_MSG \
  "Usage %s [-f <code file> | -c <code>] (-i <input file>) (-o <output file>)\n" \
  "If no input/output file is provided standart input/output is used instead.\n"


// TODO: implement proper CLI parsing
// TODO: https://github.com/CLIUtils/CLI11
int main(int argc, char **argv) {
  CLI::App app {"Simple Brainfuck interpreter written for "
                "IPL course homework from FMI, SU, 2018." };

  std::string srcFile;
  CLI::Option *srcFileOpt = app.add_option("-f,--file", srcFile, "Brainfuck source file");

  srcFileOpt->check(CLI::ExistingFile);
  
  std::string srcCode;
  CLI::Option *srcCodeOpt = app.add_option("-c,--code", srcCode, "Brainfuck source code");

  srcFileOpt->excludes(srcCodeOpt);
  srcCodeOpt->excludes(srcFileOpt);

  std::string output;
  CLI::Option *outputOpt = app.add_option("-o,--output", output, "File to write the Brainfuck output to. Optional.");

  std::string input;
  CLI::Option *inputOpt = app.add_option("-i,--input", input, "File to take the Brainfuck input from. Optional.");

  inputOpt->check(CLI::ExistingFile);

  CLI11_PARSE(app, argc, argv);

  std::string source;
  if (*srcFileOpt) {
    std::ifstream file {srcFile};
    if (file.is_open()) {
      source.assign((std::istreambuf_iterator<char>(file)),
                    std::istreambuf_iterator<char>());
    }
  } else {
    source = srcCode;
  }

  std::ifstream inputFile;
  if (*inputOpt)
    inputFile.open(input);
  
  std::ofstream outputFile;
  if (*outputOpt)
    outputFile.open(output);  
  
  std::istream &is = *inputOpt ? inputFile : std::cin;
  std::ostream &os = *outputOpt ? outputFile : std::cout;
  
  BFInterpreter bfi{source.c_str(), is, os};
  bfi.compile();

  return 0;
}
