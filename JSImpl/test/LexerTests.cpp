#include <src/Lexer.h>
#include <src/CommonTypes.h>

#include <gtest/gtest.h>

TEST(Lexer, Less)
{
	IPLVector<Token> tokens;
	Tokenize("<", tokens);

	ASSERT_EQ(tokens.size(), 2u);
        ASSERT_EQ(tokens[0].Type, TokenType::Less);
}

TEST(Lexer, Number)
{
	IPLVector<Token> tokens;
	Tokenize("213434.24", tokens);

	ASSERT_EQ(tokens.size(), 2u);
    ASSERT_EQ(tokens[0].Type, TokenType::Number);
    ASSERT_EQ(tokens[0].Number, 213434.24);
}


TEST(Lexer, HexNumber)
{
	IPLVector<Token> tokens;
	auto result = Tokenize("0x10", tokens);

	ASSERT_TRUE(result.IsSuccessful);
	ASSERT_EQ(tokens.size(), 2u);
    ASSERT_EQ(tokens[0].Type, TokenType::Number);
    ASSERT_EQ(tokens[0].Number, 16);
}

TEST(Lexer, ScientificNotationNumber)
{
	IPLVector<Token> tokens;
	auto result = Tokenize("1e2", tokens);

	ASSERT_TRUE(result.IsSuccessful);
	ASSERT_EQ(tokens.size(), 2u);
    ASSERT_EQ(tokens[0].Type, TokenType::Number);
    ASSERT_EQ(tokens[0].Number, 100);
}

TEST(Lexer, NumberStartWithNine)
{
	IPLVector<Token> tokens;
	Tokenize("999", tokens);
	ASSERT_EQ(tokens.size(), 2u);
    ASSERT_EQ(tokens[0].Type, TokenType::Number);
    ASSERT_EQ(tokens[0].Number, 999);
}

TEST(Lexer, NumberStartWithZero)
{
	IPLVector<Token> tokens;
	Tokenize("0999", tokens);
	ASSERT_EQ(tokens.size(), 2u);
    ASSERT_EQ(tokens[0].Type, TokenType::Number);
    ASSERT_EQ(tokens[0].Number, 999);
}

TEST(Lexer, SpaceNewLineSpace)
{
	IPLVector<Token> tokens;
	Tokenize(" \n var a = 4; ", tokens);
	ASSERT_TRUE(tokens.size() == 6 && tokens[0].Type == TokenType::Var &&
                tokens[1].Type == TokenType::Identifier &&
		tokens[2].Type == TokenType::Equal &&
		tokens[3].Type == TokenType::Number &&
		tokens[4].Type == TokenType::Semicolon);
}

TEST(Lexer, String)
{
	IPLVector<Token> tokens;
	Tokenize("\"alabala\"", tokens);
	ASSERT_TRUE(tokens.size() == 2 && tokens[0].Type == TokenType::String
                && tokens[0].Lexeme == "\"alabala\"");
}

TEST(Lexer, StringSingleQuotedStrings)
{
	IPLVector<Token> tokens;
	Tokenize("'alabala'", tokens);
	ASSERT_TRUE(tokens.size() == 2 && tokens[0].Type == TokenType::String && tokens[0].Lexeme == "'alabala'");
}

TEST(Lexer, KeyWord)
{
	IPLVector<Token> tokens;
	Tokenize("for", tokens);
	ASSERT_TRUE(tokens.size() == 2 && tokens[0].Type == TokenType::For);
}

TEST(Lexer, VariableDeclaration)
{
	IPLVector<Token> tokens;
	Tokenize("var pesho = 10", tokens);

	ASSERT_TRUE(tokens.size() == 5 && tokens[0].Type == TokenType::Var
		&& tokens[1].Type == TokenType::Identifier
		&& tokens[2].Type == TokenType::Equal
		&& tokens[3].Type == TokenType::Number
		&& tokens[4].Type == TokenType::Eof);
}

TEST(Lexer, StringError)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("\" aaaa", tokens);
	ASSERT_TRUE(!res.IsSuccessful && res.Error.Row == 0 && res.Error.Column == 6);
}

TEST(Lexer, TestWhiteSpaceTokens)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize(" 1\n2  abc\n \n", tokens, { true, false });
	ASSERT_TRUE(res.IsSuccessful
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

TEST(Lexer, SimpleComment)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("//comment", tokens, { false, true });
	ASSERT_TRUE(res.IsSuccessful
		&& tokens.size() == 2
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "//comment"
		&& tokens[1].Type == TokenType::Eof);
}

TEST(Lexer, SimpleMultiLineComment)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("/*comment\ncomment*/", tokens, { false, true });
	ASSERT_TRUE(res.IsSuccessful
		&& tokens.size() == 2
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "/*comment\ncomment*/"
		&& tokens[1].Type == TokenType::Eof);
}

TEST(Lexer, ConsecutiveMultiLineComment)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("/*comment1\ncomment1*//*comment2\ncomment2*/", tokens, { false, true });
	ASSERT_TRUE(res.IsSuccessful
		&& tokens.size() == 3
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "/*comment1\ncomment1*/"
		&& tokens[1].Type == TokenType::Comment
		&& tokens[1].Lexeme == "/*comment2\ncomment2*/"
		&& tokens[2].Type == TokenType::Eof);
}

TEST(Lexer, ConsecutiveMultiLineCommentWithSpace)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("/*comment1\ncomment1*/\n\n\n/*comment2\ncomment2*/", tokens, { false, true });
	ASSERT_TRUE(res.IsSuccessful
		&& tokens.size() == 3
		&& tokens[0].Type == TokenType::Comment
		&& tokens[0].Lexeme == "/*comment1\ncomment1*/"
		&& tokens[1].Type == TokenType::Comment
		&& tokens[1].Lexeme == "/*comment2\ncomment2*/"
		&& tokens[2].Type == TokenType::Eof);
}

TEST(Lexer, ConsecutiveMultiLineCommentWithSpaceWithTokens)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize("//comment1\n/*comment2\ncomment2*/\n\n\n/*comment3\ncomment3*/", tokens, { true, true });
	ASSERT_TRUE(res.IsSuccessful
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

TEST(Lexer, CommentError)
{
	IPLVector<Token> tokens;
	const auto& res = Tokenize(" aa\n /*", tokens);
	ASSERT_TRUE(!res.IsSuccessful && res.Error.Row == 1 &&
                res.Error.Column == 3);
}

TEST(Lexer, LexFile)
{
	IPLVector<Token> tokens;
	const auto& res = TokenizeFile("LexTestFile.js", tokens);

	ASSERT_TRUE(res.IsSuccessful);
	ASSERT_EQ(tokens.size(), 5);
	ASSERT_EQ(tokens[0].Type, TokenType::Var);
	ASSERT_EQ(tokens[1].Type, TokenType::Identifier);
	ASSERT_EQ(tokens[2].Type, TokenType::Equal);
	ASSERT_EQ(tokens[3].Type, TokenType::Number);
	ASSERT_EQ(tokens[4].Type, TokenType::Eof);
}

TEST(Lexer, LexBadFilePath)
{
	IPLVector<Token> tokens;
	const auto& res = TokenizeFile("BadPath.js", tokens);

	ASSERT_FALSE(res.IsSuccessful);
	ASSERT_EQ(res.Error.Row, 0u);
	ASSERT_EQ(res.Error.Column, 0u);
	ASSERT_EQ(res.Error.File, IPLString("BadPath.js"));
	ASSERT_EQ(res.Error.What, IPLString("Bad file path."));
}

TEST(Lexer, LexNotAJavaScriptFile)
{
	IPLVector<Token> tokens;
	const auto& res = TokenizeFile("NotJS.txt", tokens);

	ASSERT_FALSE(res.IsSuccessful);
	ASSERT_EQ(res.Error.Row, 0u);
	ASSERT_EQ(res.Error.Column, 0u);
	ASSERT_EQ(res.Error.File, IPLString("NotJS.txt"));
	ASSERT_EQ(res.Error.What, IPLString("Not a JavaScript file."));
}