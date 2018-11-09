#include <iostream>
int gcd(int a, int b)
{
    __asm
    {
        mov eax, a
        mov ecx, b
    loop1: 
        cmp eax, ecx
        je end
        jg suba
        jmp subb
        
    suba:
        sub eax, ecx; 
        jmp loop1; 
        
    subb:
        sub ecx, eax
        jmp loop1

    end:
    }
}

int main()
{
    std::cout << gcd(21, 5);
    return 0;
}
