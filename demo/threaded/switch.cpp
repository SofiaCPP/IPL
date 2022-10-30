#include <cstdio>
#include <cstdint>

void run_switch(uint8_t* code, int limit)
{
    int counts[] = { 0, 0, 0, 0, 0, 0, 0 };

    int ops = 0;
    int pc = 0;

    while (ops < limit)
    {
        switch (code[pc]) {
        case 0:
            {
                ++counts[0];
                ++ops;
                ++pc;
                break;
            }
        case 1:
            {
                ++counts[1];
                ++ops;
                ++pc;
                break;
            }
        case 2:
            {
                ++counts[2];
                ++ops;
                ++pc;
                break;
            }
        case 3:
            {
                ++counts[3];
                ++ops;
                ++pc;
                break;
            }
        case 4:
            {
                ++counts[4];
                ++ops;
                ++pc;
                break;
            }
        case 5:
            {
                ++counts[5];
                ++ops;
                break;
            }
        case 6:
            {
                ++counts[6];
                ++ops;
                pc = 0;
                break;
            }
        }
    }
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
    run_switch(program, 32 * 1000 * 1000);
    return 0;
}
