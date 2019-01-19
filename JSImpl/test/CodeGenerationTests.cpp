#include <src/CommonTypes.h>
#include <src/Lexer.h>
#include <src/Parser.h>
#include "src/ByteCodeGenerator.h"

#include <gtest/gtest.h>

#include <sstream>

TEST(CodeGen, Empty)
{
	IPLVector<Token> tokens;
	IPLString source = "";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 0\n"
						 "1: pop 0\n"
						 "2: halt\n";
	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, VarDeclations)
{
	IPLVector<Token> tokens;
	IPLString source = "var a;";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 1\n"
						 "1: pop 1\n"
						 "2: halt\n";
	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, VarDeclationsWithValue)
{
	IPLVector<Token> tokens;
	IPLString source = "var a = 5;";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 2\n"
						 "1: const r1 5.000000\n"
						 "2: mov r0 r1\n"
						 "3: pop 2\n"
						 "4: halt\n";
	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, VarDeclationsBinaryExpre)
{
	IPLVector<Token> tokens;
	IPLString source = "var a = 5 + 6;";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 4\n"
						 "1: const r1 6.000000\n"
						 "2: const r2 5.000000\n"
						 "3: add r3 r2 r1\n"
						 "4: mov r0 r3\n"
						 "5: pop 4\n"
						 "6: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, VariableAssignment)
{
	IPLVector<Token> tokens;
	IPLString source = "var a; a = 5;";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 2\n"
						 "1: const r1 5.000000\n"
						 "2: mov r0 r1\n"
						 "3: pop 2\n"
						 "4: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, MultiVariableAssignment)
{
	IPLVector<Token> tokens;
	IPLString source = "var a; var b = 6; a = b;";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 3\n"
						 "1: const r2 6.000000\n"
						 "2: mov r1 r2\n"
						 "3: mov r0 r1\n"
						 "4: pop 3\n"
						 "5: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, SimpleIf)
{
	IPLVector<Token> tokens;
	IPLString source = "var a = 5; if (a < 1) { a = 7}";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 5\n"
						 "1: const r1 5.000000\n"
						 "2: mov r0 r1\n"
						 "3: const r2 1.000000\n"
						 "4: less r3 r0 r2\n"
						 "5: jmpf r3 8\n"
						 "6: const r4 7.000000\n"
						 "7: mov r0 r4\n"
						 "8: pop 5\n"
						 "9: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, SimpleIfElse)
{
	IPLVector<Token> tokens;
	IPLString source = "var a = 5; if (a < 1) { a = 3; } else { a = 7; }";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 6\n"
						 "1: const r1 5.000000\n"
						 "2: mov r0 r1\n"
						 "3: const r2 1.000000\n"
						 "4: less r3 r0 r2\n"
						 "5: jmpf r3 9\n"
						 "6: const r4 3.000000\n"
						 "7: mov r0 r4\n"
						 "8: jmp 11\n"
						 "9: const r5 7.000000\n"
						 "10: mov r0 r5\n"
						 "11: pop 6\n"
						 "12: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, SimpleFor)
{
	IPLVector<Token> tokens;
	IPLString source = "var a = 0; for (var i = 0; i < 5; i++ ){ a =  a + i; }";
	Tokenize(source.c_str(), tokens);
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 9\n"
						 "1: const r1 0.000000\n"
						 "2: mov r0 r1\n"
						 "3: const r3 0.000000\n"
						 "4: mov r2 r3\n"
						 "5: const r4 5.000000\n"
						 "6: less r5 r2 r4\n"
						 "7: jmpf r5 14\n"
						 "8: add r6 r0 r2\n"
						 "9: mov r0 r6\n"
						 "10: const r7 1.000000\n"
						 "11: mov r8 r2\n"
						 "12: add r2 r2 r7\n"
						 "13: jmp 5\n"
						 "14: pop 9\n"
						 "15: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, SimpleCall)
{
        IPLVector<Token> tokens;
        IPLString source = "function f(a, b) { return 42; } var a = f(1,2);";
        Tokenize(source.c_str(), tokens);
        auto ast = Parse(tokens);
        auto asmb = GenerateByteCode(ast, source,
                                     ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
        IPLString expected =
          "0: push 8\n"
          "1: const r1 42.000000\n"
          "2: mov r0 r1\n"
          "3: ret r0\n"
          "4: const r3 1.000000\n"
          "5: mov r2 r3\n"
          "6: const r5 2.000000\n"
          "7: mov r4 r5\n"
          "8: push 3\n"
          "9: const r5 0.000000\n" // result
          "10: const r6 0.000000\n"
          "11: const r7 0.000000\n"
          "12: call 1\n"
          "13: pop 2\n"
          "14: mov r4 r7"
          "14: pop 8\n"
          "15: halt\n";

        ASSERT_TRUE(asmb == expected);
}
