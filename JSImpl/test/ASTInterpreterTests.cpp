#include "precompiled.h"

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

	bool ValueAsBool(const char* name)
	{
		return std::get<bool>(*Interpreter.ModifyVariable(name));
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

TEST_F(ASTInterpret, LessNumber)
{
	Run("var answer = 6 < 7");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_TRUE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, LessString)
{
	Run("var name = 'John' < 'Doe'");
	ASSERT_TRUE(HasVariable("name"));
	ASSERT_FALSE(ValueAsBool("name"));
}

TEST_F(ASTInterpret, StringLessNumber)
{
	ASSERT_THROW(Run("var data = 'John' < 42"), std::runtime_error);
}

TEST_F(ASTInterpret, Division)
{
	Run("var answer = 6 / 3");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_DOUBLE_EQ(ValueAsNumber("answer"), 2.0);
}

TEST_F(ASTInterpret, LessEqualEq)
{
	Run("var answer = 6 <= 6");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_TRUE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, LessEqualLess)
{
	Run("var answer = 4 <= 6");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_TRUE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, LessEqualGt)
{
	Run("var answer = 8 <= 6");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_FALSE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, GreaterEqualEq)
{
	Run("var answer = 6 >= 6");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_TRUE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, GreaterEqualLess)
{
	Run("var answer = 4 >= 6");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_FALSE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, GreaterEqualGt)
{
	Run("var answer = 8 >= 6");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_TRUE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, Greater)
{
	Run("var greater = 6 > 5;"
		"var notGreater = 5 > 6;"
		"var equal = 6 > 6"
	);
	ASSERT_TRUE(HasVariable("greater"));
	ASSERT_TRUE(ValueAsBool("greater"));

	ASSERT_TRUE(HasVariable("notGreater"));
	ASSERT_FALSE(ValueAsBool("notGreater"));

	ASSERT_TRUE(HasVariable("equal"));
	ASSERT_FALSE(ValueAsBool("equal"));
}

TEST_F(ASTInterpret, EqualEq)
{
	Run("var number = 6 == 6;"
		"var string = 'John' == 'John';"
		"var notEqual = 6 == 7"
		"var notEqualString = 'John' == 'Doe'"
	);
	ASSERT_TRUE(HasVariable("number"));
	ASSERT_TRUE(ValueAsBool("number"));

	ASSERT_TRUE(HasVariable("string"));
	ASSERT_TRUE(ValueAsBool("string"));

	ASSERT_TRUE(HasVariable("notEqual"));
	ASSERT_FALSE(ValueAsBool("notEqual"));

	ASSERT_TRUE(HasVariable("notEqualString"));
	ASSERT_FALSE(ValueAsBool("notEqualString"));
}

TEST_F(ASTInterpret, EqualNotEq)
{
	Run("var answer = 6 == 7");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_FALSE(ValueAsBool("answer"));
}

TEST_F(ASTInterpret, NotEqualEq)
{
	Run("var answer = 6 != 6;"
		"var answerString = 'John' != 'John';"
	);
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_FALSE(ValueAsBool("answer"));

	ASSERT_TRUE(HasVariable("answerString"));
	ASSERT_FALSE(ValueAsBool("answerString"));
}

TEST_F(ASTInterpret, NotEqualNotEq)
{
	Run("var answer = 6 != 7;"
		"var answerString = 'John' != 'Doe';"
	);
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_TRUE(ValueAsBool("answer"));

	ASSERT_TRUE(HasVariable("answerString"));
	ASSERT_TRUE(ValueAsBool("answerString"));
}

TEST_F(ASTInterpret, And)
{
	ASSERT_THROW(Run("var answer = true && 42"), std::runtime_error);
}

TEST_F(ASTInterpret, Substract)
{
	Run("var answer = 6 - 3");
	ASSERT_TRUE(HasVariable("answer"));
	ASSERT_DOUBLE_EQ(ValueAsNumber("answer"), 3.0);
}

TEST_F(ASTInterpret, StringToBoolTrue)
{
	Run("var answer = 'John';"
		"var number = -1;"
		"if (answer) { number = 1 } else { number = 0 }"
	);

	ASSERT_TRUE(HasVariable("number"));
	ASSERT_DOUBLE_EQ(ValueAsNumber("number"), 1.0);
}

TEST_F(ASTInterpret, StringToBoolFalse)
{
	Run("var answer = '';"
		"var number = -1;"
		"if (answer) { number = 1 } else { number = 0 }"
	);

	ASSERT_TRUE(HasVariable("number"));
	ASSERT_DOUBLE_EQ(ValueAsNumber("number"), 0.0);
}

TEST_F(ASTInterpret, While)
{
	Run("var i = 1;"
		"var s = 1;"
		"while (i < 10) {"
		"	s = s * i;"
		"	i = i + 1;"
		"}"
	);
	ASSERT_TRUE(HasVariable("i"));
	ASSERT_DOUBLE_EQ(ValueAsNumber("i"), 10.0);

	ASSERT_TRUE(HasVariable("s"));
	ASSERT_DOUBLE_EQ(ValueAsNumber("s"), 362880.0);
}