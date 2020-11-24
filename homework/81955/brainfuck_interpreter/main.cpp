#include "brainfuck.h"

#include <iostream>
#include <fstream>
#include <stack>

class Brainfuck
{
private:
    char cells[30000] = {0};
    unsigned index;
    std::stack<const char*> lp;
    
public:
    Brainfuck(unsigned i = 15000): index(i)
    {
    }

    void interpret(const char* source, std::istream& is, std::ostream& os)
    {
        for (const char* s = source; *s != 0; ++s)
        {
            switch (*s)
            {
                case '>': index++;
                    break;
                case '<': index--;
                    break;
                case '+': ++cells[index];
                    break;
                case '-': --cells[index];
                    break;
                case '.': os << (char)cells[index];
                    break;
                case ',': is >> cells[index];
                    break;
                case '[': lp.push(s);
                    break;
                case ']':
                    if (cells[index])
                        s = lp.top();
                    else
                        lp.pop();
                    break;
                default:
                    break;
            }
        }
    }

    ~Brainfuck() = default;
};

int main()
{
    std::string filename;
    std::getline(std::cin, filename);
    std::ifstream file(filename);
    std::string code_str((std::istreambuf_iterator<char>(file)), (std::istreambuf_iterator<char>()));
    Brainfuck().interpret(code_str.c_str(), std::cin, std::cout);
}   