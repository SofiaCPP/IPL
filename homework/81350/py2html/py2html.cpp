#include <iostream>
#include <string>
#include <cctype>
#include <unistd.h>
#include <sys/types.h>
#include <unordered_map>
#include <set>
#include "Token.h"

std::string operatorChars = "+-*/%=<>&|^~";
std::string parenthesisChars = "[]{}(),";
std::set<std::string> operators = {
    "+",
    "-",
    "*",
    "/",
    "//",
    "%",
    "**",
    ">",
    "<",
    ">=",
    "<=",
    "==",
    "!=",
    "&",
    "|",
    "~",
    "^",
    ">>",
    "<<",
    "=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "**=",
    "//=",
    "^=",
    "&=",
    "|=",
    ">>=",
    "<<="
};

using namespace std;
FILE* in;
FILE* out;

bool isNumber(char c){
    return c >= '0' && c <= '9';
}

bool notValidNumber(char c){
    return !(isNumber(c) || c == '.');
}

bool isQuote(char c){
    return c == '\'' || c == '"';
}

bool isLastQuote(char firstQuote, char previousChar, char currentChar){
    return firstQuote == currentChar && previousChar != '\\';
}

bool isOperator(char c){
    return operatorChars.find(c) != std::string::npos;
}

bool isParenthesis(char c){
    return parenthesisChars.find(c) != std::string::npos;
}

bool isValidIdentifierChar(char c){
    return !(isOperator(c) || isParenthesis(c) || isQuote(c) || c == '\n' || isblank(c) || c == ',');
}

bool isValidOperator(std::string word){
    std::set<std::string>::iterator iter;
    iter = operators.find(word);
    return iter != operators.end();
}

std::string readFormat(){
    std::string word = "{";
    char c;

    while((c = fgetc(in)) != EOF && c != '}'){
        word += c;
    }
    word += '}';
    return word;
}

std::string readNumber(){
    fseek(in, -1, SEEK_CUR);
    std::string word;
    char c;
    int dotCount = 0;

    while((c = fgetc(in)) != EOF && !notValidNumber(c)){
        //printf("%c", c);
        if(notValidNumber(c)){
            return "";
        } else if(c == '.'){
            dotCount++;
            if(dotCount >= 2){
                return "";
            }
        }
        word += c;
    }
    if(notValidNumber(c)){
        fseek(in, -1, SEEK_CUR);
    }
    return word;
}

std::string readString(char firstQuote){
    std::string word = "";
    word += firstQuote;
    char c;
    char previousChar = ' ';

    while((c = fgetc(in)) != EOF && !isLastQuote(firstQuote, previousChar, c)){
        if( c == '{'){
            fprintf(out, "<span class=\"string\">%s</span>", word.c_str());
            word = readFormat();
            fprintf(out, "<span class=\"formatString\">%s</span>", word.c_str());
            word = "";
        } else {
            word += c;
        }
        previousChar = c;
    }
    word += firstQuote;
    fprintf(out, "<span class=\"string\">%s</span>", word.c_str());
    return word;
}

std::string readOperator(){
    fseek(in, -1, SEEK_CUR);
    std::string word;
    char c;
    while((c = fgetc(in)) != EOF && isOperator(c)){
        word += c;
    }
    if(!isOperator(c)){
        fseek(in, -1, SEEK_CUR);
    }
    return word;
}

std::string readWord(){
    fseek(in, -1, SEEK_CUR);
    std::string word;
    char c;
    while((c = fgetc(in)) != EOF && isValidIdentifierChar(c)){
        word += c;
     }

     if(!isValidIdentifierChar(c)){
        fseek(in, -1, SEEK_CUR);
     }
    return word;
}

void lexer(){
    char c;
    bool validSyntaxis = 1;
    std::string word;
    std::unordered_map<std::string, TokenType>::const_iterator iter;

    while((c = fgetc(in)) != EOF){
        if(isQuote(c)){
            word = readString(c);
            //fprintf(out, "<span class=\"string\">%s</span>", word.c_str());
            continue;
        }
        if(isNumber(c)){
            word = readNumber();
            fprintf(out, "<span class=\"number\">%s</span>", word.c_str());
            continue;
        }
        if(isOperator(c)){
            word = readOperator();
            if(isValidOperator(word)){
                validSyntaxis = 0;
            }
            fprintf(out, "<span class=\"operator\">%s</span>", word.c_str());
            // trqbwa da se proweti dali e korektno
            // t.e. da e w nqkyw set ili hash
            continue;
        }
        if(isParenthesis(c)){
            fprintf(out, "%c", c);
            continue;
        }
        if(isblank(c) || c == '\n'){
            fprintf(out, "%c", c);
            continue;
        }

        word = readWord();
        iter = keywords.find(word);
        if(iter == keywords.end()){
            fprintf(out, "%s", word.c_str());
        } else {
            fprintf(out, "<span class=\"keyword\">%s</span>", word.c_str());
        }

    }
}

int main(int argc, const char* argv[])
{
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
            in = fopen( argv[0], "r" );
    else
            in = stdin;

    out = fopen("index.html", "w");

    fputs(
        "<!doctype>"
        "<html>"
        "<head>"
        "    <title>program.py</title>"
        "    <style>"
        "        .keyword {"
        "            color: purple;"
        "        }"
        "        .number {"
        "            color: orange;"
        "        }"
        "        .string {"
        "            color: green;"
        "        }"
        "        .operator {"
        "            font-style: bold;"
        "        }"
        "        .formatString {"
        "            color: red;"
        "        }"
        "    </style>"
        "</head>"
        "<body>"
        "    <pre class=\"code\">",
        out);

    lexer();
    fputs("</pre></body></html>", out);
    fclose(in);
    fclose(out);
    return 0;
}
