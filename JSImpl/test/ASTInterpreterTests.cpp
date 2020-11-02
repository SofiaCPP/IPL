#include <gtest/gtest.h>

#include <src/CommonTypes.h>
#include <src/Lexer.h>
#include <src/Parser.h>
#include <src/ASTPrinter.h>
#include <src/ASTInterpreter.h>

struct ASTInterpret : ::testing::Test
{
	void Run(const char* program)
	{
		auto tokens = Tokenize(program).tokens;
		auto expr = Parse(tokens);
		Interpreter.Run(expr.get());
	}

	bool HasVariable(const char* name)
	{
		return Interpreter.HasVariable(name);
	}

	double ValueAsNumber(const char* name)
	{
		return std::get<double>(*Interpreter.ModifyVariable(name));
	}

	IPLString ValueAsString(const char* name)
	{
		return std::get<IPLString>(*Interpreter.ModifyVariable(name));
	}

	ASTInterpreter Interpreter;
};

TEST_F(ASTInterpret, VarNumber)
{
	Run("var answer = 6 * 7");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_DOUBLE_EQ(ValueAsNumber("answer"), 42.0);
}

TEST_F(ASTInterpret, VarString)
{
	Run("var name = 'John' + ' ' + 'Doe'");
	ASSERT_TRUE(HasVariable("name"));
	ASSERT_EQ(ValueAsString("name"), "John Doe");
}

TEST_F(ASTInterpret, StringPlusNumberNotSupported)
{
	ASSERT_THROW(Run("var data = 'John Doe' + 42"), std::runtime_error);
}

TEST_F(ASTInterpret, StringMinusString)
{
	ASSERT_THROW(Run("var data = 'John' - 'Doe'"), std::runtime_error);
}
