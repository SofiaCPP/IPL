#include <cstdio>
#include <cstdint>

#include "program.h"

#define CC __attribute__((preserve_none))

struct Input
{
    uint8_t* code;
    int limit;
};

struct State
{
    int pc;
    int ops;
};

CC void run(uint8_t* code, int limit, State& state, int* counts);
CC void add(uint8_t* code, int limit, State& state, int* counts);
CC void sub(uint8_t* code, int limit, State& state, int* counts);
CC void mul(uint8_t* code, int limit, State& state, int* counts);
CC void div(uint8_t* code, int limit, State& state, int* counts);
CC void print  (uint8_t* code, int limit, State& state, int* counts);
CC void halt   (uint8_t* code, int limit, State& state, int* counts);
CC void restart(uint8_t* code, int limit, State& state, int* counts);


typedef CC void (*Instruction)(uint8_t* code, int limit, State& state, int* counts);

Instruction table[7] = {
    add,
    sub,
    mul,
    div,
    print,
    halt,
    restart,
};

CC void run(uint8_t* code, int limit, State& state, int* counts)
{
    [[clang::musttail]] return table[code[state.pc]](code, limit, state, counts);
}

CC void add(uint8_t* code, int limit, State& state, int* counts)
{
    ++counts[0];
    if (++state.ops == limit) return;
    [[clang::musttail]] return table[code[++state.pc]](code, limit, state, counts);
}

CC void sub(uint8_t* code, int limit, State& state, int* counts)
{
    ++counts[1];
    if (++state.ops == limit) return;
    [[clang::musttail]] return table[code[++state.pc]](code, limit, state, counts);
}

CC void mul(uint8_t* code, int limit, State& state, int* counts)
{
    ++counts[2];
    if (++state.ops == limit) return;
    [[clang::musttail]] return table[code[++state.pc]](code, limit, state, counts);
}

CC void div(uint8_t* code, int limit, State& state, int* counts)
{
    ++counts[3];
    if (++state.ops == limit) return;
    [[clang::musttail]] return table[code[++state.pc]](code, limit, state, counts);
}

CC void print(uint8_t* code, int limit, State& state, int* counts)
{
    ++counts[4];
    if (++state.ops == limit) return;
    [[clang::musttail]] return table[code[++state.pc]](code, limit, state, counts);
}

CC void halt(uint8_t* code, int limit, State& state, int* counts)
{
    ++counts[5];
    if (++state.ops == limit) return;
}

CC void restart(uint8_t* code, int limit, State& state, int* counts)
{
    ++counts[6];
    if (++state.ops == limit) return;
    state.pc = 0;
    [[clang::musttail]] return table[code[state.pc]](code, limit, state, counts);
}



void run_musttail(uint8_t* code, int limit)
{

    int counts[] = { 0, 0, 0, 0, 0, 0, 0 };

    State state {0, 0};

    run(code, limit, state, counts);

#if !defined(NO_PRINT)
    std::printf("add: %d\n", counts[0]);
    std::printf("sub: %d\n", counts[1]);
    std::printf("mul: %d\n", counts[2]);
    std::printf("div: %d\n", counts[3]);
    std::printf("print: %d\n", counts[4]);
    std::printf("halt: %d\n", counts[5]);
    std::printf("restart: %d\n", counts[6]);
#endif
}

int main()
{
    run_musttail(program, 32 * 1000 * 1000);
    return 0;
}
