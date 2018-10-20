#include "Lexer.h"
#include "Parser.h"
#include "ASTPrinter.h"
#include <iostream>
#define LOG(msg) std::cout << msg << std::endl
#define EXECUTE_TEST(test) std::cout << #test << " "; test();

#define CHECK(cond)	if (cond) {LOG("PASS");} else {LOG(#cond " FAIL");}

// Lexer Tests
void TestLess()
{
	IPLVector<Token> tokens;
	Tokenize("<", tokens, { false, false });

	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Less);
}

void TestNumber()
{
	IPLVector<Token> tokens;
	Tokenize("213434.24", tokens, { false, false });

	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Number && tokens[0].Number == 213434.24);
}

void TestNumberStartWithNine()
{
	IPLVector<Token> tokens;
	Tokenize("999", tokens, { false, false });
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Number && tokens[0].Number == 999);
}

void TestNumberStartWithZero()
{
	IPLVector<Token> tokens;
	Tokenize("0999", tokens, { false });
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::Number && tokens[0].Number == 999);
}

void TestSpaceNewLineSpace()
{
	IPLVector<Token> tokens;
	Tokenize(" \n var a = 4; ", tokens, { false, false });
	CHECK(tokens.size() == 6 && tokens[0].Type == TokenType::Var &&
		tokens[1].Type == TokenType::Identifier &&
		tokens[2].Type == TokenType::Equal &&
		tokens[3].Type == TokenType::Number &&
		tokens[4].Type == TokenType::Semicolon);
}

void TestString()
{
	IPLVector<Token> tokens;
	Tokenize("\"alabala\"", tokens, { false, false });
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::String && tokens[0].Lexeme == "\"alabala\"");
}

void TestStringSingleQuotedStrings()
{
	IPLVector<Token> tokens;
	Tokenize("'alabala'", tokens, { false, false });
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::String && tokens[0].Lexeme == "'alabala'");
}

void TestKeyWord()
{
	IPLVector<Token> tokens;
	Tokenize("for", tokens, { false, false });
	CHECK(tokens.size() == 2 && tokens[0].Type == TokenType::For);
}

void TestVariableDeclaration()
{
	IPLVector<Token> tokens;
	Tokenize("var pesho = 10", tokens, { false, false });

	CHECK(tokens.size() == 5 && tokens[0].Type == TokenType::Var
		&& tokens[1].Type == TokenType::Identifier
		&& tokens[2].Type == TokenType::Equal
		&& tokens[3].Type == TokenType::Number
		&& tokens[4].Type == TokenType::Eof);
}

void TestStringError()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("\" aaaa", tokens);
	CHECK(!res.IsSuccessful && res.Error.Row == 0 && res.Error.Column == 6);
}

void TestWhiteSpaceTokens()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize(" 1\n2  abc\n \n", tokens, { true, false });
	CHECK(res.IsSuccessful
		&& tokens.size() == 8
		&& tokens[0].Type == TokenType::Whitespace
		&& tokens[0].Lexeme == " "
		&& tokens[1].Type == TokenType::Number
		&& tokens[1].Number == 1
		&& tokens[2].Type == TokenType::Whitespace
		&& tokens[2].Lexeme == "\n"
		&& tokens[3].Type == TokenType::Number
		&& tokens[3].Number == 2
		&& tokens[4].Type == TokenType::Whitespace
		&& tokens[4].Lexeme == "  "
		&& tokens[5].Type == TokenType::Identifier
		&& tokens[5].Lexeme == "abc"
		&& tokens[6].Type == TokenType::Whitespace
		&& tokens[6].Lexeme == "\n \n"
		&& tokens[7].Type == TokenType::Eof);
}

void TestSimpleComment()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("//comment", tokens, { false, true });
	CHECK(res.IsSuccessful
		&& tokens.size() == 2
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "//comment"
		&& tokens[1].Type == TokenType::Eof);
}

void TestSimpleMultiLineComment()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("/*comment\ncomment*/", tokens, { false, true });
	CHECK(res.IsSuccessful
		&& tokens.size() == 2
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "/*comment\ncomment*/"
		&& tokens[1].Type == TokenType::Eof);
}

void TestConsecutiveMultiLineComment()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("/*comment1\ncomment1*//*comment2\ncomment2*/", tokens, { false, true });
	CHECK(res.IsSuccessful
		&& tokens.size() == 3
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "/*comment1\ncomment1*/"
		&& tokens[1].Type == TokenType::Comment
		&& tokens[1].Lexeme == "/*comment2\ncomment2*/"
		&& tokens[2].Type == TokenType::Eof);
}

void TestConsecutiveMultiLineCommentWithSpace()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("/*comment1\ncomment1*/\n\n\n/*comment2\ncomment2*/", tokens, { false, true });
	CHECK(res.IsSuccessful
		&& tokens.size() == 3
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "/*comment1\ncomment1*/"
		&& tokens[1].Type == TokenType::Comment
		&& tokens[1].Lexeme == "/*comment2\ncomment2*/"
		&& tokens[2].Type == TokenType::Eof);
}

void TestConsecutiveMultiLineCommentWithSpaceWithTokens()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("//comment1\n/*comment2\ncomment2*/\n\n\n/*comment3\ncomment3*/", tokens, { true, true });
	CHECK(res.IsSuccessful
		&& tokens.size() == 6
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "//comment1"
		&& tokens[1].Type == TokenType::Whitespace
		&& tokens[1].Lexeme == "\n"
		&& tokens[2].Type == TokenType::Comment
		&& tokens[2].Lexeme == "/*comment2\ncomment2*/"
		&& tokens[3].Type == TokenType::Whitespace
		&& tokens[3].Lexeme == "\n\n\n"
		&& tokens[4].Type == TokenType::Comment
		&& tokens[4].Lexeme == "/*comment3\ncomment3*/"
		&& tokens[5].Type == TokenType::Eof);
}

void TestCommentError()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize(" aa\n /*", tokens, { false, true });
	CHECK(!res.IsSuccessful && res.Error.Row == 1 && res.Error.Column == 3);
}

// Parser Tests
void TestParseUnaryExpr()
{
	IPLVector<Token> tokens;
	Tokenize("function pesho() { var a = 0; return a; }", tokens, { false });

	// TODO make actual test :D
	auto expr = Parse(tokens);
	ASTPrinter p(std::cout);
	expr->Accept(p);
}

void TestHexNumber()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("0x10", tokens);

	CHECK(res.IsSuccessful && tokens[0].Type == TokenType::Number && tokens[0].Number == 16);
}

void TestScientificNotationNumber()
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("1e2", tokens);

	CHECK(res.IsSuccessful && tokens[0].Type == TokenType::Number && tokens[0].Number == 100);
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
	EXECUTE_TEST(TestWhiteSpaceTokens);
	EXECUTE_TEST(TestSimpleComment);
	EXECUTE_TEST(TestSimpleMultiLineComment);
	EXECUTE_TEST(TestConsecutiveMultiLineComment);
	EXECUTE_TEST(TestConsecutiveMultiLineCommentWithSpace);
	EXECUTE_TEST(TestConsecutiveMultiLineCommentWithSpaceWithTokens);
	EXECUTE_TEST(TestCommentError);
	EXECUTE_TEST(TestHexNumber);
	EXECUTE_TEST(TestScientificNotationNumber);

	// TestParseUnaryExpr();
#if defined(_WIN32)
	std::system("pause");
#endif
	return 0;
}
