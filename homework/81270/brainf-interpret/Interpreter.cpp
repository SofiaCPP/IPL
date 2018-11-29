//
// Created by bmestanov on 15.11.18.
//

#define TAPE_SIZE 65536
#define STACK_SIZE 8192

#include <fstream>
#include <iostream>

void failIf(bool condition, const char *message) {
    if (condition) {
        std::cerr << message;
        exit(1);
    }
}

int main(int argc, char **argv) {
    argc--;
    argv++;
    char memory[TAPE_SIZE] = {0};
    char *pp = memory; // program pointer
    long stack[STACK_SIZE];
    int sp = 0; // stack pointer

    failIf(!argv[0], "No file provided");

    std::ifstream file;
    file.open(argv[0]);

    failIf(!file, "Error opening file");

    while (!file.eof()) {
        auto symbol = static_cast<char>(file.get());
        switch (symbol) {
            case '>': {
                // Allow memory wrap
                pp = memory + (pp - memory + 1) % TAPE_SIZE;
                break;
            }
            case '<': {
                pp = memory + (pp - memory - 1) % TAPE_SIZE;
                break;
            }
            case '+': {
                (*pp)++;
                break;
            }
            case '-': {
                (*pp)--;
                break;
            }
            case '.': {
                std::putchar(*pp);
                break;
            }
            case ',': {
                *pp = static_cast<char>(std::getchar());
                break;
            }
            case '[': {
                if (*pp) {
                    failIf(sp == STACK_SIZE, "Stack overflow");
                    stack[sp++] = file.tellg();
                } else {
                    long move = 1;
                    while (move > 0) {
                        auto next = static_cast<char>(file.get());
                        if (next == '[') move++;
                        if (next == ']') move--;
                        if (file.eof()) failIf(true, "Invalid syntax");
                    }
                }
                break;
            }
            case ']': {
                failIf(sp == 0, "Invalid syntax");
                if (*pp) {
                    long top = stack[sp - 1];
                    file.seekg(top);
                } else {
                    sp--;
                }
            }
            default:;
        }
    }

    return 0;
}