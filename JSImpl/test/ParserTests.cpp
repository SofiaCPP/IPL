#include <src/CommonTypes.h>
#include <src/Lexer.h>
#include <src/Parser.h>
#include <src/ASTPrinter.h>
#include <src/ASTInterpreter.h>

#include <gtest/gtest.h>

#include <sstream>

TEST(Parser, ParseUnaryExpr)
{
	IPLVector<Token> tokens = Tokenize("function pesho() { var a = 0; return a; } var a = \"3\";").tokens;

	// TODO make actual test :D
	auto expr = Parse(tokens);
    std::ostringstream output;
	PrintAST(expr, output);
	ASSERT_TRUE(!output.str().empty());
}

TEST(Parser, VariableDeclaration)
{
	IPLVector<Token> tokens = Tokenize("var s = 0;").tokens;

	auto expr = Parse(tokens);
	ASTInterpreter i;
	i.Run(expr.get());
	ASSERT_TRUE(i.HasVariable("s"));
	ASSERT_TRUE(i.ModifyVariable("s") == 0.0);
}

TEST(Parser, Assignment)
{
	IPLVector<Token> tokens = Tokenize("var s = 0; s = 100;").tokens;

	auto expr = Parse(tokens);
	ASTInterpreter i;
	i.Run(expr.get());
	ASSERT_TRUE(i.HasVariable("s"));
	ASSERT_TRUE(i.ModifyVariable("s") == 100);
}

TEST(Parser, VariableDeclarationAdditionOfLiterals)
{
	IPLVector<Token> tokens = Tokenize("var s = 5 + 6;").tokens;

	auto expr = Parse(tokens);
	ASTInterpreter i;
	i.Run(expr.get());
	ASSERT_TRUE(i.HasVariable("s"));
	ASSERT_DOUBLE_EQ(i.ModifyVariable("s"), 11.0);
}

TEST(Parser, VariableDeclarationAdditionOfVariables)
{
	IPLVector<Token> tokens = Tokenize("var a = 5; var b = 4; var c = a + b;").tokens;

	auto expr = Parse(tokens);
	ASTInterpreter i;
	i.Run(expr.get());
	ASSERT_TRUE(i.HasVariable("a"));
	ASSERT_TRUE(i.HasVariable("b"));
	ASSERT_TRUE(i.HasVariable("c"));
	ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 5.0);
	ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 4.0);
	ASSERT_DOUBLE_EQ(i.ModifyVariable("c"), 9.0);
}

TEST(Parser, VariableDeclarationMultiplicationOfLiterals)
{
	IPLVector<Token> tokens = Tokenize("var s = 5 * 6;").tokens;

	auto expr = Parse(tokens);
	ASTInterpreter i;
	i.Run(expr.get());
	ASSERT_TRUE(i.HasVariable("s"));
	ASSERT_DOUBLE_EQ(i.ModifyVariable("s"), 30.0);
}

TEST(Parser, VariableDeclarationMultiplicationOfVariables)
{
	IPLVector<Token> tokens = Tokenize("var a = 5; var b = 4; var c = a * b;").tokens;

	auto expr = Parse(tokens);
	ASTInterpreter i;
	i.Run(expr.get());
	ASSERT_TRUE(i.HasVariable("a"));
	ASSERT_TRUE(i.HasVariable("b"));
	ASSERT_TRUE(i.HasVariable("c"));
	ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 5.0);
	ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 4.0);
	ASSERT_DOUBLE_EQ(i.ModifyVariable("c"), 20.0);
}


TEST(Parser, Unary)
{
	IPLVector<Token> tokens = Tokenize("var a = 5; a++; var b = 6; --b; var minusB = -b; var plusB = +b;").tokens;

	auto expr = Parse(tokens);
	ASTInterpreter i;
	i.Run(expr.get());
	ASSERT_TRUE(i.HasVariable("a"));
	ASSERT_TRUE(i.HasVariable("b"));
	ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 6.0);
	ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 5.0);
	ASSERT_DOUBLE_EQ(i.ModifyVariable("minusB"), -5.0);
	ASSERT_DOUBLE_EQ(i.ModifyVariable("plusB"),  +5.0);
}


TEST(Parser, If)
{
	{
		IPLVector<Token> tokens = Tokenize("var a = 5; var b = 3; if(a == 5){a++; b++;} b++;").tokens;

		auto expr = Parse(tokens);
		ASTInterpreter i;
		i.Run(expr.get());
		ASSERT_TRUE(i.HasVariable("a"));
		ASSERT_TRUE(i.HasVariable("b"));
		ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 6.0);
		ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 5.0);
	}
	{
		IPLVector<Token> tokens = Tokenize("var a = 5; var b = 3; if(a == 5)a++; b++; b++;").tokens;

		auto expr = Parse(tokens);
		ASTInterpreter i;
		i.Run(expr.get());
		ASSERT_TRUE(i.HasVariable("a"));
		ASSERT_TRUE(i.HasVariable("b"));
		ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 6.0);
		ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 5.0);
	}

	{
		IPLVector<Token> tokens = Tokenize("var a = 5; var b = 3; if(a != 5)a++; b++; b++;").tokens;

		auto expr = Parse(tokens);
		ASTInterpreter i;
		i.Run(expr.get());
		ASSERT_TRUE(i.HasVariable("a"));
		ASSERT_TRUE(i.HasVariable("b"));
		ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 5.0);
		ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 5.0);
	}

	{
		IPLVector<Token> tokens = Tokenize("var a = 5; var b = 3; if(a != 5){a++;} else { b++;}b++;").tokens;

		auto expr = Parse(tokens);
		ASTInterpreter i;
		i.Run(expr.get());
		ASSERT_TRUE(i.HasVariable("a"));
		ASSERT_TRUE(i.HasVariable("b"));
		ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 5.0);
		ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 5.0);
	}

	{
		IPLVector<Token> tokens = Tokenize("var a = 5; var b = 3; if(a == 5){a++;} else { b++;}b++;").tokens;

		auto expr = Parse(tokens);
		ASTInterpreter i;
		i.Run(expr.get());
		ASSERT_TRUE(i.HasVariable("a"));
		ASSERT_TRUE(i.HasVariable("b"));
		ASSERT_DOUBLE_EQ(i.ModifyVariable("a"), 6.0);
		ASSERT_DOUBLE_EQ(i.ModifyVariable("b"), 4.0);
	}
}

TEST(Parser, For)
{
	{
		IPLVector<Token> tokens = Tokenize("var i = 0; for (var j = 0; j < 10; j++) { i = i + j; }").tokens;

		auto expr = Parse(tokens);
		ASTInterpreter i;
		i.Run(expr.get());
		ASSERT_TRUE(i.HasVariable("i"));
		ASSERT_DOUBLE_EQ(i.ModifyVariable("i"), 45.0);
	}
	{
		IPLVector<Token> tokens = Tokenize("var i = 0; for (var j = 0; j < 10; j++) i = i + j;").tokens;
		auto expr = Parse(tokens);
		ASTInterpreter i;
		i.Run(expr.get());
		ASSERT_TRUE(i.HasVariable("i"));
		ASSERT_DOUBLE_EQ(i.ModifyVariable("i"), 45.0);
	}
}

TEST(Parser, ObjectLiter)
{
	// for now just test for crashes :D 
	{
		IPLVector<Token> tokens = Tokenize("var i = {}").tokens;
		auto expr = Parse(tokens);
	}

	{
		IPLVector<Token> tokens = Tokenize("var i = {a: 4}").tokens;
		auto expr = Parse(tokens);
	}

	{
		IPLVector<Token> tokens = Tokenize("var i = {a: 4, b: 5}").tokens;
		auto expr = Parse(tokens);
	}

	{
		IPLVector<Token> tokens = Tokenize("var i = {a: 4, b: {a : 5}}").tokens;
		auto expr = Parse(tokens);
	}
}

