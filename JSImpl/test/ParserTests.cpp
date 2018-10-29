#include <src/CommonTypes.h>
#include <src/Lexer.h>
#include <src/Parser.h>
#include <src/ASTPrinter.h>

#include <gtest/gtest.h>

#include <sstream>

TEST(Parser, ParseUnaryExpr)
{
	IPLVector<Token> tokens;
	Tokenize("function pesho() { var a = 0; return a; } var a = \"3\";", tokens);

	// TODO make actual test :D
	auto expr = Parse(tokens);
        std::ostringstream output;
	ASTPrinter p(output);
	expr->Accept(p);
        ASSERT_TRUE(!output.str().empty());
}

