#include <src/Lexer.h>
#include <src/CommonTypes.h>

#include <gtest/gtest.h>

TEST(Lexer, Less)
{
	IPLVector<Token> tokens = Tokenize("<").tokens;

	ASSERT_EQ(tokens.size(), 2u);
	 ASSERT_EQ(tokens[0].Type, TokenType::Less);
}

TEST(Lexer, Number)
{
	IPLVector<Token> tokens = Tokenize("213434.24").tokens;

	ASSERT_EQ(tokens.size(), 2u);
	ASSERT_EQ(tokens[0].Type, TokenType::Number);
	ASSERT_EQ(tokens[0].Number, 213434.24);
}


TEST(Lexer, HexNumber)
{
	const auto res = Tokenize("0x10");
	const auto& tokens = res.tokens;

	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_EQ(tokens.size(), 2u);
	ASSERT_EQ(tokens[0].Type, TokenType::Number);
	ASSERT_EQ(tokens[0].Number, 16);
}

TEST(Lexer, ScientificNotationNumber)
{
	const auto res = Tokenize("1e2");
	const auto& tokens = res.tokens;

	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_EQ(tokens.size(), 2u);
	ASSERT_EQ(tokens[0].Type, TokenType::Number);
	ASSERT_EQ(tokens[0].Number, 100);
}

TEST(Lexer, NumberStartWithNine)
{
	IPLVector<Token> tokens = Tokenize("999").tokens;
	ASSERT_EQ(tokens.size(), 2u);
	ASSERT_EQ(tokens[0].Type, TokenType::Number);
	ASSERT_EQ(tokens[0].Number, 999);
}

TEST(Lexer, NumberStartWithZero)
{
	IPLVector<Token> tokens = Tokenize("0999").tokens;
	ASSERT_EQ(tokens.size(), 2u);
	ASSERT_EQ(tokens[0].Type, TokenType::Number);
	ASSERT_EQ(tokens[0].Number, 999);
}

TEST(Lexer, SpaceNewLineSpace)
{
	IPLVector<Token> tokens = Tokenize(" \n var a = 4; ").tokens;
	ASSERT_TRUE(tokens.size() == 6 && tokens[0].Type == TokenType::Var);
	ASSERT_TRUE(tokens[1].Type == TokenType::Identifier);
	ASSERT_TRUE(tokens[2].Type == TokenType::Equal);
	ASSERT_TRUE(tokens[3].Type == TokenType::Number);
	ASSERT_TRUE(tokens[4].Type == TokenType::Semicolon);
}

TEST(Lexer, String)
{
	IPLVector<Token> tokens = Tokenize("\"alabala\"").tokens;
	ASSERT_TRUE(tokens.size() == 2 && tokens[0].Type == TokenType::String 
				&& tokens[0].Lexeme == "\"alabala\"");
}

TEST(Lexer, StringSingleQuotedStrings)
{
	IPLVector<Token> tokens = Tokenize("'alabala'").tokens;
	ASSERT_TRUE(tokens.size() == 2 && tokens[0].Type == TokenType::String && tokens[0].Lexeme == "'alabala'");
}

TEST(Lexer, KeyWord)
{
	IPLVector<Token> tokens = Tokenize("for").tokens;
	ASSERT_TRUE(tokens.size() == 2 && tokens[0].Type == TokenType::For);
}

TEST(Lexer, VariableDeclaration)
{
	IPLVector<Token> tokens = Tokenize("var pesho = 10").tokens;

	ASSERT_TRUE(tokens.size() == 5 && tokens[0].Type == TokenType::Var);
	ASSERT_TRUE(tokens[1].Type == TokenType::Identifier);
	ASSERT_TRUE(tokens[2].Type == TokenType::Equal);
	ASSERT_TRUE(tokens[3].Type == TokenType::Number);
	ASSERT_TRUE(tokens[4].Type == TokenType::Eof);
}

TEST(Lexer, StringError)
{
	const auto res = Tokenize("\" aaaa");
	ASSERT_TRUE(!res.IsSuccessful && res.Error.Row == 0 && res.Error.Column == 6);
}

TEST(Lexer, TestWhiteSpaceTokens)
{
	const auto res = Tokenize(" 1\n2  abc\n \n", { true, false });
	const auto& tokens = res.tokens;
	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_TRUE(tokens.size() == 11);
	ASSERT_TRUE(tokens[0].Type == TokenType::Whitespace);
	ASSERT_TRUE(tokens[0].Lexeme == "");
	ASSERT_TRUE(tokens[1].Type == TokenType::Number);
	ASSERT_TRUE(tokens[1].Number == 1);
	ASSERT_TRUE(tokens[2].Type == TokenType::NewLine);
	ASSERT_TRUE(tokens[2].Lexeme == "");
	ASSERT_TRUE(tokens[3].Type == TokenType::Number);
	ASSERT_TRUE(tokens[3].Number == 2);
	ASSERT_TRUE(tokens[4].Type == TokenType::Whitespace);
	ASSERT_TRUE(tokens[4].Lexeme == "");
	ASSERT_TRUE(tokens[5].Type == TokenType::Whitespace);
	ASSERT_TRUE(tokens[5].Lexeme == "");
	ASSERT_TRUE(tokens[6].Type == TokenType::Identifier);
	ASSERT_TRUE(tokens[6].Lexeme == "abc");
	ASSERT_TRUE(tokens[7].Type == TokenType::NewLine);
	ASSERT_TRUE(tokens[7].Lexeme == "");
	ASSERT_TRUE(tokens[8].Type == TokenType::Whitespace);
	ASSERT_TRUE(tokens[8].Lexeme == "");
	ASSERT_TRUE(tokens[9].Type == TokenType::NewLine);
	ASSERT_TRUE(tokens[9].Lexeme == "");
	ASSERT_TRUE(tokens[10].Type == TokenType::Eof);
}

TEST(Lexer, SimpleComment)
{
	const auto res = Tokenize("//comment", { false, true });
	const auto& tokens = res.tokens;
	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_TRUE(tokens.size() == 2);
	ASSERT_TRUE(tokens[0].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[0].Lexeme == "//comment");
	ASSERT_TRUE(tokens[1].Type == TokenType::Eof);
}

TEST(Lexer, SimpleMultiLineComment)
{
	const auto res = Tokenize("/*comment\ncomment*/", { false, true });
	const auto& tokens = res.tokens;
	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_TRUE(tokens.size() == 2);
	ASSERT_TRUE(tokens[0].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[0].Lexeme == "/*comment\ncomment*/");
	ASSERT_TRUE(tokens[1].Type == TokenType::Eof);
}

TEST(Lexer, ConsecutiveMultiLineComment)
{
	const auto res = Tokenize("/*comment1\ncomment1*//*comment2\ncomment2*/", { false, true });
	const auto& tokens = res.tokens;
	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_TRUE(tokens.size() == 3);
	ASSERT_TRUE(tokens[0].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[0].Lexeme == "/*comment1\ncomment1*/");
	ASSERT_TRUE(tokens[1].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[1].Lexeme == "/*comment2\ncomment2*/");
	ASSERT_TRUE(tokens[2].Type == TokenType::Eof);
}

TEST(Lexer, ConsecutiveMultiLineCommentWithSpace)
{
	const auto res = Tokenize("/*comment1\ncomment1*/\n\n\n/*comment2\ncomment2*/", { false, true });
	const auto& tokens = res.tokens;
	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_TRUE(tokens.size() == 3);
	ASSERT_TRUE(tokens[0].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[0].Lexeme == "/*comment1\ncomment1*/");
	ASSERT_TRUE(tokens[1].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[1].Lexeme == "/*comment2\ncomment2*/");
	ASSERT_TRUE(tokens[2].Type == TokenType::Eof);
}

TEST(Lexer, ConsecutiveMultiLineCommentWithSpaceWithTokens)
{
	const auto res = Tokenize("//comment1\n/*comment2\ncomment2*/\n\n\n/*comment3\ncomment3*/", { true, true });
	const auto& tokens = res.tokens;
	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_TRUE(tokens.size() == 8);
	ASSERT_TRUE(tokens[0].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[0].Lexeme == "//comment1");
	ASSERT_TRUE(tokens[1].Type == TokenType::NewLine);
	ASSERT_TRUE(tokens[1].Lexeme == "");
	ASSERT_TRUE(tokens[2].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[2].Lexeme == "/*comment2\ncomment2*/");
	ASSERT_TRUE(tokens[3].Type == TokenType::NewLine);
	ASSERT_TRUE(tokens[3].Lexeme == "");
	ASSERT_TRUE(tokens[4].Type == TokenType::NewLine);
	ASSERT_TRUE(tokens[4].Lexeme == "");
	ASSERT_TRUE(tokens[5].Type == TokenType::NewLine);
	ASSERT_TRUE(tokens[5].Lexeme == "");
	ASSERT_TRUE(tokens[6].Type == TokenType::Comment);
	ASSERT_TRUE(tokens[6].Lexeme == "/*comment3\ncomment3*/");
	ASSERT_TRUE(tokens[7].Type == TokenType::Eof);
}

TEST(Lexer, CommentError)
{
	auto res = Tokenize(" aa\n /*");
	ASSERT_TRUE(!res.IsSuccessful && res.Error.Row == 1 &&
				res.Error.Column == 3);
}

