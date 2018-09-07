#include "Lexer.h"
#include "Parser.h"
#include <iostream>
#define LOG(msg) std::cout << msg << std::endl
#define EXECUTE_TEST(test) std::cout << #test << " "; test();

// Lexer Tests
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

void TestVariableDeclaration()
{
	auto tokens = Tokenize("var pesho = 10");
	if (tokens.size() == 5 && tokens[0].Type == TokenType::Var
		&& tokens[1].Type == TokenType::Identifier
		&& tokens[2].Type == TokenType::Equal
		&& tokens[3].Type == TokenType::Number
		&& tokens[4].Type == TokenType::Eof)
	{
		LOG("PASS");
	}
	else
	{
		LOG("FAIL");
	}
}
// Parser Tests
void TestParseUnaryExpr()
{
	// TODO make actual test :D
	auto expr = Parse(Tokenize("--10"));
	expr->Print(std::cout);
}

int main()
{
	EXECUTE_TEST(TestLess);
	EXECUTE_TEST(TestNumber);
	EXECUTE_TEST(TestString);
	EXECUTE_TEST(TestKeyWord);
	EXECUTE_TEST(TestVariableDeclaration);
	EXECUTE_TEST(TestParseUnaryExpr);
	std::system("pause");
	return 0;
}