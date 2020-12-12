#include <cstdio>
#include <cstdint>

extern "C" int64_t add_answer(int64_t x);

int main() {
    std::printf("The result is %lld\n", add_answer(24));
    return 0;
}
