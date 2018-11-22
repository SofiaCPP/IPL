#include <gtest/gtest.h>

#include <spasm.hpp>
#include <assembler.hpp>
#include <sstream>

using Spasm::OpCodes;

struct SPRTTest : public ::testing::Test
{
    typedef std::vector<Spasm::byte> ByteCode;

    void Run(const ByteCode& code)
    {
        Run(code.data(), code.size());
    }

    void Run(const Spasm::byte* bytecode, size_t size)
    {
        VM.Initialize(size, bytecode, Input, Output);
        ASSERT_EQ(Spasm::Spasm::RunResult::Success, VM.run());
    }
    Spasm::byte Label(Spasm::byte label) const {
        return 0x80 + label;
    }
    Spasm::Spasm VM;
    std::istringstream Input;
    std::ostringstream Output;
};

struct SPASMTest : public SPRTTest
{
	void CompileAndRun(const std::string& program)
	{
		SpasmImpl::ASM::Bytecode_Memory bytecode;
		std::istringstream programInput;
		programInput.str(program);
		ASSERT_TRUE(SpasmImpl::ASM::compile(programInput, bytecode))
			<< "Could not compile the program:" << std::endl << program;
		Run(bytecode.bytecode());
	}
};

TEST_F(SPRTTest, Empty)
{
}

TEST_F(SPRTTest, Add)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::Add, 3, 1, 2,
		OpCodes::Print, 3,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "13");
}

TEST_F(SPRTTest, Sub)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::Sub, 3, 1, 2,
		OpCodes::Print, 3,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "-1");
}

TEST_F(SPRTTest, Div)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::Div, 3, 1, 2,
		OpCodes::Print, 3,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "0.857143");
}

TEST_F(SPRTTest, Mod)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::Mod, 3, 1, 2,
		OpCodes::Print, 3,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "6");
}

TEST_F(SPRTTest, Mutliply)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::Mul, 3, 1, 2,
		OpCodes::Print, 3,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "42");
}

TEST_F(SPRTTest, Less)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::Less, 3, 1, 2,
		OpCodes::Less, 4, 2, 1,
		OpCodes::Less, 5, 2, 2,
		OpCodes::Print, 3,
		OpCodes::Print, 4,
		OpCodes::Print, 5,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "100");
}


TEST_F(SPRTTest, LessEq)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::LessEq, 3, 1, 2,
		OpCodes::LessEq, 4, 2, 1,
		OpCodes::LessEq, 5, 2, 2,
		OpCodes::Print, 3,
		OpCodes::Print, 4,
		OpCodes::Print, 5,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "101");
}

TEST_F(SPRTTest, Jump)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,
		OpCodes::Const, 2, 7,
		OpCodes::LessEq, 3, 1, 2,
		OpCodes::LessEq, 4, 2, 1,
                OpCodes::Jump, 18,
		OpCodes::Print, 5,
		OpCodes::Print, 3,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "1");
}

TEST_F(SPRTTest, JumpTrue)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,   // 3
		OpCodes::Const, 2, 7,   // 6
		OpCodes::Less, 3, 1, 2, // 10
                OpCodes::JumpT, 3, 17,  // 13
		OpCodes::Print, 2,      // 15
                OpCodes::Jump, 19,      // 17
		OpCodes::Print, 1,      // 19
                OpCodes::Halt,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "6");
}

TEST_F(SPRTTest, JumpFalse)
{
	Spasm::byte bytecode[] = {
		OpCodes::Const, 1, 6,   // 3
		OpCodes::Const, 2, 7,   // 6
		OpCodes::Less, 3, 1, 2, // 10
                OpCodes::JumpF, 3, 17,  // 13
		OpCodes::Print, 2,      // 15
                OpCodes::Jump, 19,      // 17
		OpCodes::Print, 1,      // 19
                OpCodes::Halt,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "7");
}

TEST_F(SPRTTest, Call)
{
	Spasm::byte bytecode[] = {
		OpCodes::Push, 5,      // 2
		OpCodes::Const, 1, 0,   // 5
		OpCodes::Const, 2, 6,   // 8
		OpCodes::Const, 3, 7,   // 11
		OpCodes::Const, 4, 2,   // 14
		OpCodes::PushFrom, 3,       // 16
		OpCodes::PushFrom, 2,       // 18
		OpCodes::PushFrom, 4,       // 20
		OpCodes::Call, 25,      // 22
		OpCodes::Print, 4,      // 24
        OpCodes::Halt,          // 25
		OpCodes::Print, 0,      // 27
		OpCodes::Print, -1,     // 29
		OpCodes::Print, -2,     // 31
		OpCodes::Mul, 1, -2, -1,// 33
        OpCodes::Ret, 1,        // 35
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "26742");
}

TEST_F(SPRTTest, Read)
{
	Spasm::byte bytecode[] = {
		OpCodes::Read, 1,
		OpCodes::Print, 1,
	};

        Input.str("42");
	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "42");
}

TEST_F(SPASMTest, RSyntax)
{
	const char* program =
		"00: push 5"		"\n"
		"01: const r1 0"	"\n"
		"02: const r2 6"	"\n"
		"03: const r3 7"	"\n"
		"04: const r4 2"	"\n"
		"05: pushr r3"		"\n"
		"06: pushr r2"		"\n"
		"07: pushr r4"		"\n"
		"08: call mult # *"	"\n"
		"09: popr r1"		"\n"
		"10: print r1"		"\n"
		"11: halt"			"\n"
		"12: label mult"	"\n"
		"13: print a0"		"\n"
		"14: print a1"		"\n"
		"15: print a2"		"\n"
		"16: mul r1 a2 a1"	"\n"
		"17: ret r1"		"\n"
		""
		;
	CompileAndRun(program);
	ASSERT_EQ(Output.str(), "26742");
}

TEST_F(SPASMTest, Call)
{
	const char* program =
		"push 5"		"\n"
		"const 1 0"		"\n"
		"const 2 6"		"\n"
		"const 3 7"		"\n"
		"const 4 2"		"\n"
		"pushr 3"		"\n"
		"pushr 2"		"\n"
		"pushr 4"		"\n"
		"call mult"		"\n"
		"print 4"		"\n"
		"halt"			"\n"
		"label mult"	"\n"
		"print 0"		"\n"
		"print -1"		"\n"
		"print -2"		"\n"
		"mul 1 -2 -1"	"\n"
		"ret 1"			"\n"
		""
		;
	CompileAndRun(program);
	ASSERT_EQ(Output.str(), "26742");
}

TEST_F(SPASMTest, GCD)
{
	const char* program =
		"push 3"		"\n"
		"read 1"		"\n"
		"read 2"		"\n"
		"label loop"	"\n"
		"less 3 1 2"	"\n"
		"jmpt 3 sub_ba"	"\n"
		"less 3 2 1"	"\n"
		"jmpt 3 sub_ab"	"\n"
		"print 1"		"\n"
		"halt"			"\n"
		"label sub_ab"	"\n"
		"sub 1 1 2"		"\n"
		"jmp loop"		"\n"
		"label sub_ba"	"\n"
		"sub 2 2 1"		"\n"
		"jmp loop"		"\n"
		""
		;
	Input.str("21 12");
	CompileAndRun(program);
	ASSERT_EQ(Output.str(), "3");
}
