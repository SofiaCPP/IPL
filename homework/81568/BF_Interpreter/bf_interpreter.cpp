#include "bf_interpreter.h"

#include <cmath>
#include <cstring>
#include <iostream>
#include <stack>

#ifndef MEMSIZE
#define MEMSIZE 30000
#endif

BFInterpreter::BFInterpreter(const char *program, std::istream &input, std::ostream &output) :
  memory_blocks(nullptr), p(nullptr),
  program(nullptr), input(input), output(output),
  memory_size(MEMSIZE) {
  
  this->program = new char[strlen(program) + 1];
  strncpy(this->program, program, strlen(program) + 1);
  pp = this->program;

  memory_blocks = new uchar[memory_size];
  for (int i = 0; i < memory_size; ++i) {
    memory_blocks[i] = 0;
  }
  p = memory_blocks;
}

BFInterpreter::~BFInterpreter() {
  delete [] memory_blocks;
  delete [] program;
}

void BFInterpreter::addMemory() {
  uchar *new_memory = new uchar[memory_size + ((size_t) log2(memory_size))];
  for (int i = 0; i < memory_size; ++i) {
    new_memory[i] = memory_blocks[i];
  }
  delete [] memory_blocks;
  memory_blocks = new_memory;
  memory_size += log2(memory_size);
}

void BFInterpreter::compile() {
  while (*pp) {
    switch (*pp) {
    case '[':
      ++pp;
      loop();
      break;
    case ']': // that will only happen if code is wrong
      printf("Syntax error!\n");
      return;
    default:
      proccessNonLoopCmd(*pp);
    }
    ++pp;
  }
  output.put('\n');
}

void BFInterpreter::loop() {
  std::stack<char*> loops;
  loops.push(pp);
  
  char *body = loops.top();
  while (!loops.empty()) {
    while (*body != ']' && *body != '[') {
      proccessNonLoopCmd(*body);
      ++body;
    }
    if (*body == ']') {
      if (!*p) { /* End of a loop */
        loops.pop();
        ++body;
      } else { /* Loop has not ended. Return to the beginning of the current loop */
        body = loops.top();
      }
    } else { /* Start of a new loop */
      loops.push(body + 1);
      ++body;
    }
  }
  pp = --body;
  return;
}

void BFInterpreter::proccessNonLoopCmd(char c) {
  switch (c) {
  case '+':
    ++*p;
    break;
  case '-':
    --*p;
    break;
  case '.':
    output.put(*p);
    break;
  case ',':
    *p = input.get();
    break;
  case '<':
    if (p == memory_blocks)
      p = memory_blocks + (memory_size - 1);
    else 
      --p;
    break;
  case '>':
    if (p == memory_blocks + (memory_size - 1))
      p = memory_blocks;
    else
      ++p;
    break;
  }
}
