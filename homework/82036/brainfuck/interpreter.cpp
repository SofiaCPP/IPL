#include <iostream>
#include <string>
#include <sstream>
#include <stack>
#include <fstream>

std::string clear_input(std::string filename)
{
    std::ifstream in{filename};
    std::stringstream stream{};
    stream << in.rdbuf();
    std::string str = stream.str();
    
    std::string input;
    for (int i = 0; i < str.size(); i++)
    {
        char c = str[i];
        if (c == '+' || c == '-' || c == '>' || c == '<' || c == '.' || c == ',' || c == '[' || c == ']')
        {
            input += c;
        }
    }
    return input;
}

void interpret(std::string code, std::istream &input, std::ostream &output)
{
    int id = 0;
    char *ptr = new char[100000]{0};
    std::stack<int> loops_start_positions;
    for (int i = 0; i < code.size(); i++)
    {
        char c = code[i];
        if (c == '>')
        {
            ++id;
        }
        else if (c == '<')
        {
            --id;
        }
        else if (c == '+')
        {
            ++(ptr[id]);
        }
        else if (c == '-')
        {
            --(ptr[id]);
        }
        else if (c == '.')
        {
            output << ptr[id];
        }
        else if (c == ',')
        {
            input >> ptr[id];
        }
        else if (c == '[')
        {
            if (ptr[id] != 0)
            {
                loops_start_positions.push(i);
            }
        }
        else if (c == ']')
        {
            auto pos = loops_start_positions.top();
            if (ptr[id] == 0)
            {
                loops_start_positions.pop();
            }
            else
            {
                i = pos;
            }
        }
    }
}

int main()
{
    std::string filename;
    std::cin >> filename;
    std::string input = clear_input(filename);
    interpret(input, std::cin, std::cout);
    return 0;
}