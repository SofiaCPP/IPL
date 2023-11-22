#include "precompiled.h"
#include <src/CommonTypes.h>
#include <src/Lexer.h>
#include <src/Parser.h>
#include "src/ByteCodeGenerator.h"

#include <sstream>

TEST(CodeGen, Empty)
{
	IPLString source = "";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a;";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a = 5;";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a = 5 + 6;";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a; a = 5;";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a; var b = 6; a = b;";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a = 5; if (a < 1) { a = 7}";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a = 5; if (a < 1) { a = 3; } else { a = 7; }";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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
	IPLString source = "var a = 0; for (var i = 0; i < 5; i++ ){ a =  a + i; }";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
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

TEST(CodeGen, EmptyObject)
{
	IPLString source = "var a = {}";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 2\n"
						 "1: mov r0 r1\n"
						 "2: pop 2\n"
						 "3: halt\n";

	ASSERT_TRUE(asmb == expected);
}


TEST(CodeGen, ObjectWithFlatProps)
{
	IPLString source = "var a = {a: 5, n: \"pesho\"}";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 6\n"
						 "1: const r2 5.000000\n"
						 "2: string r3 a\n"
						 "3: set r1 r3 r2\n"
						 "4: string r4 pesho\n"
						 "5: string r5 n\n"
						 "6: set r1 r5 r4\n"
						 "7: mov r0 r1\n"
						 "8: pop 6\n"
						 "9: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, NestedObjects)
{
	IPLString source = "var a = {a: 5, n: \"pesho\", obj1: {a: 4, b: 5}}\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 12\n"
						 "1: const r2 5.000000\n"
						 "2: string r3 a\n"
						 "3: set r1 r3 r2\n"
						 "4: string r4 pesho\n"
						 "5: string r5 n\n"
						 "6: set r1 r5 r4\n"
						 "7: const r7 4.000000\n"
						 "8: string r8 a\n"
						 "9: set r6 r8 r7\n"
						 "10: const r9 5.000000\n"
						 "11: string r10 b\n"
						 "12: set r6 r10 r9\n"
						 "13: string r11 obj1\n"
						 "14: set r1 r11 r6\n"
						 "15: mov r0 r1\n"
						 "16: pop 12\n"
						 "17: halt\n";

	ASSERT_TRUE(asmb == expected);
}


TEST(CodeGen, ObjectGetters)
{
	IPLString source = "var a = {c: 3}; var k = a.c.t;\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 9\n"
		"1: const r2 3.000000\n"
		"2: string r3 c\n"
		"3: set r1 r3 r2\n"
		"4: mov r0 r1\n"
		"5: string r5 c\n"
		"6: get r0 r5 r6\n"
		"7: string r7 t\n"
		"8: get r6 r7 r8\n"
		"9: mov r4 r8\n"
		"10: pop 9\n"
		"11: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, ObjectSetter)
{
	IPLString source = "var a = {c: 3}; a.c = 5;\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 7\n"
		"1: const r2 3.000000\n"
		"2: string r3 c\n"
		"3: set r1 r3 r2\n"
		"4: mov r0 r1\n"
		"5: const r4 5.000000\n"
		"6: string r5 c\n"
		"7: get r0 r5 r6\n"
		"8: mov r6 r4\n"
		"9: pop 7\n"
		"10: halt\n";

	ASSERT_TRUE(asmb == expected);
}


TEST(CodeGen, BasicDoWhile)
{
	IPLString source = "var a = 5; do{ a--; }while(a != 0)\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 6\n"
		"1: const r1 5.000000\n"
		"2: mov r0 r1\n"
		"3: const r2 1.000000\n"
		"4: mov r3 r0\n"
		"5: sub r0 r0 r2\n"
		"6: const r4 0.000000\n"
		"7: neq r5 r0 r4\n"
		"8: jmpt r5 3\n"
		"9: pop 6\n"
		"10: halt\n";

	ASSERT_TRUE(asmb == expected);
}


TEST(CodeGen, BasicWhile)
{
	IPLString source = "var a = 5; while(a != 0){ a--; }\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 6\n"
		"1: const r1 5.000000\n"
		"2: mov r0 r1\n"
		"3: const r2 0.000000\n"
		"4: neq r3 r0 r2\n"
		"5: jmpf r3 10\n"
		"6: const r4 1.000000\n"
		"7: mov r5 r0\n"
		"8: sub r0 r0 r4\n"
		"9: jmp 3\n"
		"10: pop 6\n"
		"11: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, ContinueDoWhile)
{
	IPLString source = "var a = 5;\n"
						"do {\n"
						"		a--;\n"
						"		if(a == 4) continue;\n"
						"}while(a != 0)\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 8\n"
						"1: const r1 5.000000\n"
						"2: mov r0 r1\n"
						"3: const r2 1.000000\n"
						"4: mov r3 r0\n"
						"5: sub r0 r0 r2\n"
						"6: const r4 4.000000\n"
						"7: eq r5 r0 r4\n"
						"8: jmpf r5 10\n"
						"9: jmp 3\n"
						"10: const r6 0.000000\n"
						"11: neq r7 r0 r6\n"
						"12: jmpt r7 3\n"
						"13: pop 8\n"
						"14: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, BreakDoWhile)
{
	IPLString source = "var a = 5;\n"
						"do {\n"
						"		a--;\n"
						"		if(a == 4) break;\n"
						"}while(a != 0)\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 8\n"
						"1: const r1 5.000000\n"
						"2: mov r0 r1\n"
						"3: const r2 1.000000\n"
						"4: mov r3 r0\n"
						"5: sub r0 r0 r2\n"
						"6: const r4 4.000000\n"
						"7: eq r5 r0 r4\n"
						"8: jmpf r5 10\n"
						"9: jmp 13\n"
						"10: const r6 0.000000\n"
						"11: neq r7 r0 r6\n"
						"12: jmpt r7 3\n"
						"13: pop 8\n"
						"14: halt\n";

	ASSERT_TRUE(asmb == expected);
}


TEST(CodeGen, BreakWhile)
{
	IPLString source = "var a = 5;\n"
		"while(a != 0) {\n"
		"		a--;\n"
		"		if(a == 4) break;\n"
		"}\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 8\n"
		"1: const r1 5.000000\n"
		"2: mov r0 r1\n"
		"3: const r2 0.000000\n"
		"4: neq r3 r0 r2\n"
		"5: jmpf r3 14\n"
		"6: const r4 1.000000\n"
		"7: mov r5 r0\n"
		"8: sub r0 r0 r4\n"
		"9: const r6 4.000000\n"
		"10: eq r7 r0 r6\n"
		"11: jmpf r7 13\n"
		"12: jmp 14\n"
		"13: jmp 3\n"
		"14: pop 8\n"
		"15: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, ContinueWhile)
{
	IPLString source = "var a = 5;\n"
		"while(a != 0) {\n"
		"		a--;\n"
		"		if(a == 4) continue;\n"
		"}\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 8\n"
		"1: const r1 5.000000\n"
		"2: mov r0 r1\n"
		"3: const r2 0.000000\n"
		"4: neq r3 r0 r2\n"
		"5: jmpf r3 14\n"
		"6: const r4 1.000000\n"
		"7: mov r5 r0\n"
		"8: sub r0 r0 r4\n"
		"9: const r6 4.000000\n"
		"10: eq r7 r0 r6\n"
		"11: jmpf r7 13\n"
		"12: jmp 3\n"
		"13: jmp 3\n"
		"14: pop 8\n"
		"15: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, ContinueFor)
{
	IPLString source = "for (var i = 0; i < 5; i++ ){ if(i == 1) continue; }\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 8\n"
		"1: const r1 0.000000\n"
		"2: mov r0 r1\n"
		"3: const r2 5.000000\n"
		"4: less r3 r0 r2\n"
		"5: jmpf r3 14\n"
		"6: const r4 1.000000\n"
		"7: eq r5 r0 r4\n"
		"8: jmpf r5 10\n"
		"9: jmp 10\n"
		"10: const r6 1.000000\n"
		"11: mov r7 r0\n"
		"12: add r0 r0 r6\n"
		"13: jmp 3\n"
		"14: pop 8\n"
		"15: halt\n";

	ASSERT_TRUE(asmb == expected);
}

TEST(CodeGen, BreakFor)
{
	IPLString source = "for (var i = 0; i < 5; i++ ){ if(i == 1) break; }\n";
	IPLVector<Token> tokens = Tokenize(source.c_str()).tokens;
	auto ast = Parse(tokens);
	auto asmb = GenerateByteCode(ast, source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, false));
	IPLString expected = "0: push 8\n"
		"1: const r1 0.000000\n"
		"2: mov r0 r1\n"
		"3: const r2 5.000000\n"
		"4: less r3 r0 r2\n"
		"5: jmpf r3 14\n"
		"6: const r4 1.000000\n"
		"7: eq r5 r0 r4\n"
		"8: jmpf r5 10\n"
		"9: jmp 14\n"
		"10: const r6 1.000000\n"
		"11: mov r7 r0\n"
		"12: add r0 r0 r6\n"
		"13: jmp 3\n"
		"14: pop 8\n"
		"15: halt\n";

	ASSERT_TRUE(asmb == expected);
}
