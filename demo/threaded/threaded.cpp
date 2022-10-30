#include <cstdio>
#include <cstdint>

void run_threaded(uint8_t* code, int limit)
{
    static void* opjump[] = {
        &&add,
        &&sub,
        &&mul,
        &&div,
        &&print,
        &&halt,
        &&restart,
    };

    int counts[] = { 0, 0, 0, 0, 0, 0, 0 };

    int ops = 0;
    int pc = 0;
    goto *opjump[code[pc]];

    while (1)
    {
add:
        {
            ++counts[0];
            if (++ops == limit) break;
            goto *opjump[code[++pc]];
        }
sub:
        {
            ++counts[1];
            if (++ops == limit) break;
            goto *opjump[code[++pc]];
        }
mul:
        {
            ++counts[2];
            if (++ops == limit) break;
            goto *opjump[code[++pc]];
        }
div:
        {
            ++counts[3];
            if (++ops == limit) break;
            goto *opjump[code[++pc]];
        }
print:
        {
            ++counts[4];
            if (++ops == limit) break;
            goto *opjump[code[++pc]];
        }
halt:
        {
            ++counts[5];
            if (++ops == limit) break;
            break;
        }

restart:
        {
            ++counts[6];
            if (++ops == limit) break;
            pc = 0;
            goto *opjump[code[pc]];
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
    run_threaded(program, 32 * 1000 * 1000);
    return 0;
}
