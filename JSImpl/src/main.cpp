#include "Lexer.h"
#include <iostream>
#define LOG(msg) std::cout << msg << std::endl
void TestLess()
{
	auto tokens = Tokenize("<");
	if (tokens.size() == 2 && tokens[0].Type == TokenType::Less)
	{
		LOG("PASS");
	}
	else
	{
		LOG("FAIL");
	}
}

void TestNumber()
{
	auto tokens = Tokenize("213434.24");
	if (tokens.size() == 2 && tokens[0].Type == TokenType::Number && tokens[0].Number == 213434.24)
	{
		LOG("PASS");
	}
	else
	{
		LOG("FAIL");
	}
}

void TestString()
{
	auto tokens = Tokenize("\"alabala\"");
	if (tokens.size() == 2 && tokens[0].Type == TokenType::String && tokens[0].Lexeme == "\"alabala\"")
	{
		LOG("PASS");
	}
	else
	{
		LOG("FAIL");
	}
}

void TestKeyWord()
{
	auto tokens = Tokenize("for");
	if (tokens.size() == 2 && tokens[0].Type == TokenType::For)
	{
		LOG("PASS");
	}
	else
	{
		LOG("FAIL");
	}
}

int main()
{
	TestLess();
	TestNumber();
	TestString();
	TestKeyWord();
	return 0;
}