#include <iostream>
#include <string>
#include <cstring>

std::string colors[] = {"#000000", "#000000", "#0c863a", "#f9d121", "#da1a2e", "#581845", "#000000", "#3b17f3"};

bool is_space(const char *&token)
{
    return strcmp(token, " ") == 0;
}

bool is_newline(const char *&token)
{
    return strcmp(token, "\n") == 0;
}

std::string get_html_tag(int id, const char *&token)
{
    if (is_newline(token))
    {
        return "<br>";
    }
    std::string span;
    span.append("<span style=\"color:");
    span.append(colors[id]);
    span.append(";\">");
    if (is_space(token))
    {
        span.append("&nbsp;");
    }
    else
    {
        span.append(token);
    }
    return span;
}

void print_token(int id, const char *token, FILE *out)
{
    fputs(get_html_tag(id, token).c_str(), out);
}