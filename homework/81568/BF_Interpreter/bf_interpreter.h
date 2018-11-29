#ifndef BRAINFUCK_INTERPRETER_H
#define BRAINFUCK_INTERPRETER_H

#include <cstddef>
#include <iostream>


class BFInterpreter {
public:
  using uchar = unsigned char;
  
  BFInterpreter(const char *program, std::istream &input = std::cin, std::ostream &output = std::cout);
  ~BFInterpreter();

  void compile();

private:
  void addMemory();
  void loop();
  void proccessNonLoopCmd(char c);
  
private:
  std::istream &input;
  std::ostream &output;
  size_t memory_size;
  
  uchar *memory_blocks;
  uchar *p;
  char *program;
  char *pp;
};

#endif /* BRAINFUCK_INTERPRETER_H */
