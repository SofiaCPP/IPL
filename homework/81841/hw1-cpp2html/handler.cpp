#include "definitions.h"
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <sstream>

using namespace std;

struct token_type_hash
{
    size_t operator () (token_type type)const{return (size_t)type;}
};

const unordered_map<token_type, string, token_type_hash> colors{
    {token_type::TT_KEYWORD, "#0000ff"},
    {token_type::TT_STANDARD_TYPEDEF, "#00ffee"},
    {token_type::TT_LITERAL_TEXT, "#f06c6c"},
    {token_type::TT_LITERAL_NUMERIC, "#c421bf"},
    {token_type::TT_UNKNOWN, "#000000"},
    {token_type::TT_WHITESPACE, "#000000"},
    {token_type::TT_COMMENT, "#56fc03"},
    {token_type::TT_DIRECTIVE, "#706e26"},
    {token_type::TT_IDENTIFIER, "#000000"},
};

string escape_html(string text)
{
    //return text;
    stringstream builder;

    for(char character : text)
    {
        string toPrint = "_";
        toPrint[0]=character;

        switch(character)
        {
        case ' ': toPrint="&nbsp;"; break;
        case '\t': toPrint="&nbsp;&nbsp;&nbsp;&nbsp;"; break;
        case '\n': toPrint="<br/>"; break;
        case '\r': toPrint=""; break;
        case '<': toPrint="&lt;"; break;
        case '>': toPrint="&gt;"; break;
        default: break;
        }

        builder<<toPrint;
    }

    string result = builder.str();
    return result;
}

void handle_token(token_type tokenType, const char *text)
{
    string color = "#ffffff";
	auto foundColor = colors.find(tokenType);
	if(foundColor!=colors.end())color=foundColor->second;

	cout<<"<span style=\"color: "<<color<<"\">"<<escape_html(text)<<"</span>";
}

int main()
{
	cout<<R"###(
    <!doctype html>
    <html>
        <head>
        </head>
        <body>
	)###";

	lex();

	cout<<R"###(
        </body>
    </html>
	)###";
}
