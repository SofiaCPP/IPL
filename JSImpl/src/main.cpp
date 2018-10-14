#include "Lexer.h"
#include "Parser.h"
#include <iostream>
#define LOG(msg) std::cout << msg << std::endl
#define EXECUTE_TEST(test) std::cout << #test << " "; test();
#define CHECK(cond) cond ? LOG("PASS") : LOG("FAIL")

// Lexer Tests
void TestLess()
{
	IPLVector<Token> tokens;
	Tokenize("<", tokens);

	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Less);
}

void TestNumber()
{
	IPLVector<Token> tokens;
	Tokenize("213434.24", tokens);

	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Number && tokens[0].Number == 213434.24);
}

void TestNumberStartWithNine()
{
	IPLVector<Token> tokens;
	Tokenize("999", tokens);
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Number && tokens[0].Number == 999);
}

void TestNumberStartWithZero()
{
	IPLVector<Token> tokens;
	Tokenize("0999", tokens);
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Number && tokens[0].Number == 999);
}

void TestSpaceNewLineSpace()
{
	IPLVector<Token> tokens;
	Tokenize(" \n var a = 4; ", tokens);
	CHECK(tokens.size() == 6 && tokens[0].Type == TokenType::Var &&
		tokens[1].Type == TokenType::Identifier &&
		tokens[2].Type == TokenType::Equal &&
		tokens[3].Type == TokenType::Number &&
		tokens[4].Type == TokenType::Semicolon);
}

void TestString()
{
	IPLVector<Token> tokens;
	Tokenize("\"alabala\"", tokens);
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::String && tokens[0].Lexeme == "\"alabala\"");
}

void TestStringSingleQuotedStrings()
{
	IPLVector<Token> tokens;
	Tokenize("'alabala'", tokens);
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::String && tokens[0].Lexeme == "'alabala'");
}

void TestKeyWord()
{
	IPLVector<Token> tokens;
	Tokenize("for", tokens);
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::For);
}

void TestVariableDeclaration()
{
	IPLVector<Token> tokens;
	Tokenize("var pesho = 10", tokens);

	CHECK(tokens.size() == 5 && tokens[0].Type == TokenType::Var
		&& tokens[1].Type == TokenType::Identifier
		&& tokens[2].Type == TokenType::Equal
		&& tokens[3].Type == TokenType::Number
		&& tokens[4].Type == TokenType::Eof);
}

void TestStringError()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("\"aaaa", tokens);

	CHECK(!res.IsSuccessful && res.Error.Row == 0 && res.Error.Column == 5);
}

// Parser Tests
void TestParseUnaryExpr()
{
	IPLVector<Token> tokens;
	Tokenize("function pesho() { var a = 0; return a; }", tokens);

	// TODO make actual test :D
	auto expr = Parse(tokens);
}

int main()
{
	EXECUTE_TEST(TestLess);
	EXECUTE_TEST(TestNumber);
	EXECUTE_TEST(TestNumberStartWithNine);
	EXECUTE_TEST(TestNumberStartWithZero);
	EXECUTE_TEST(TestString);
	EXECUTE_TEST(TestSpaceNewLineSpace);
	EXECUTE_TEST(TestStringSingleQuotedStrings);
	EXECUTE_TEST(TestKeyWord);
	EXECUTE_TEST(TestVariableDeclaration);
	EXECUTE_TEST(TestStringError);

	//EXECUTE_TEST(TestParseUnaryExpr);
#if defined(_WIN32)
	std::system("pause");
#endif
	return 0;
}
