#include <cstdio>
#include <cstdint>

void run(uint8_t* code, int limit, int& pc, int& ops, int* counts);
void add(uint8_t* code, int limit, int& pc, int& ops, int* counts);
void sub(uint8_t* code, int limit, int& pc, int& ops, int* counts);
void mul(uint8_t* code, int limit, int& pc, int& ops, int* counts);
void div(uint8_t* code, int limit, int& pc, int& ops, int* counts);
void print(uint8_t* code, int limit, int& pc, int& ops, int* counts);
void halt(uint8_t* code, int limit, int& pc, int& ops, int* counts);
void restart(uint8_t* code, int limit, int& pc, int& ops, int* counts);


typedef void (*Instruction)(uint8_t* code, int limit, int& pc, int& ops, int* counts);

Instruction table[7] = {
    add,
    sub,
    mul,
    div,
    print,
    halt,
    restart,
};

void run(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    [[clang::musttail]] return table[code[pc]](code, limit, pc, ops, counts);
}

void add(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    ++counts[0];
    if (++ops == limit) return;
    [[clang::musttail]] return table[code[++pc]](code, limit, pc, ops, counts);
}

void sub(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    ++counts[1];
    if (++ops == limit) return;
    [[clang::musttail]] return table[code[++pc]](code, limit, pc, ops, counts);
}

void mul(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    ++counts[2];
    if (++ops == limit) return;
    [[clang::musttail]] return table[code[++pc]](code, limit, pc, ops, counts);
}

void div(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    ++counts[3];
    if (++ops == limit) return;
    [[clang::musttail]] return table[code[++pc]](code, limit, pc, ops, counts);
}

void print(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    ++counts[4];
    if (++ops == limit) return;
    [[clang::musttail]] return table[code[++pc]](code, limit, pc, ops, counts);
}

void halt(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    ++counts[5];
    if (++ops == limit) return;
}

void restart(uint8_t* code, int limit, int& pc, int& ops, int* counts)
{
    ++counts[6];
    if (++ops == limit) return;
    pc = 0;
    [[clang::musttail]] return table[code[pc]](code, limit, pc, ops, counts);
}



void run_musttail(uint8_t* code, int limit)
{

    int counts[] = { 0, 0, 0, 0, 0, 0, 0 };

    int ops = 0;
    int pc = 0;

    run(code, limit, pc, ops, counts);

    std::printf("add: %d\n", counts[0]);
    std::printf("sub: %d\n", counts[1]);
    std::printf("mul: %d\n", counts[2]);
    std::printf("div: %d\n", counts[3]);
    std::printf("print: %d\n", counts[4]);
    std::printf("halt: %d\n", counts[5]);
    std::printf("restart: %d\n", counts[6]);
}

int main()
{
    uint8_t program[] = {
        0, 0, 0, 0,
        1, 2, 1, 2,
        4, 3, 4, 3,
        6,
    };
    run_musttail(program, 32 * 1000 * 1000);
    return 0;
}
