#include <stdio.h>
#include <stack>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string>

using namespace std;

class Brainfuck {
    private:
        static const int init_length = 1024;
        char *tape;
        stack<int> loop_stack;
        char *tape_pos;

    public:
        Brainfuck() : tape(new char[init_length]), tape_pos(tape + (init_length / 2)) {}
        Brainfuck(int _length): tape(new char [_length]), tape_pos(tape + (_length / 2)) {}

        void interpret(string& code) {
            for (int i = 0; code[i]; ++i) {
                switch (code[i]) {
                    case '\n': break;
                    case '<': --(this->tape_pos); break;
                    case '>': ++(this->tape_pos); break;
                    case '-': -- *(this->tape_pos); break;
                    case '+': ++ *(this->tape_pos); break;
                    case '[': this->loop_stack.push(i); break;
                    case ']':
                        if (*(this->tape_pos)) {
                            i = this->loop_stack.top();
                        } else {
                            this->loop_stack.pop();
                        }
                        break;
                    case '.': putchar(*this->tape_pos); break;
                    case ',': *(this->tape_pos) = (char)getchar();
                    default: break;
                }
            }
        }
};

int main(int argc, char *argv[])
{
    Brainfuck bf;
    if (argc != 2) {
        printf("Usage: <executable> <text_file_name>\n");
        return 1;
    }

    int fd;
    if (!(fd = open(argv[1], O_RDONLY))) {
        printf("Error opening file \"%s\"\n", argv[1]);
        return 1;
    };

    char buff[128];
    string code;
    while (int lth = read(fd, buff, 128)) {
        buff[lth] = '\0';
        code += buff;
    }

    bf.interpret(code);
    return 0;
}