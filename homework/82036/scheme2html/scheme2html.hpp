#include <iostream>
#include <string>
#include <cstring>
#include <map>
#include "AST.hpp"

AST ast{};

void put_token(int id, const char *token)
{
    ast.put_token(id, token);
}

void print(FILE *out)
{
    ast.print(out);
}