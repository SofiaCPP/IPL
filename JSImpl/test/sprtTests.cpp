#include <gtest/gtest.h>

#include <spasm.hpp>
#include <sstream>

using Spasm::OpCodes;

struct SPRTTest : public ::testing::Test
{
    typedef std::vector<Spasm::byte> ByteCode;

    void Run(ByteCode& code)
    {
        Run(code.data(), code.size());
    }

    void Run(Spasm::byte* bytecode, size_t size)
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
	ASSERT_EQ(Output.str(), "0");
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
		OpCodes::Alloc, 5,      // 2
		OpCodes::Const, 1, 0,   // 5
		OpCodes::Const, 2, 6,   // 8
		OpCodes::Const, 3, 7,   // 11
		OpCodes::Const, 4, 2,   // 14
		OpCodes::Push, 3,       // 16
		OpCodes::Push, 2,       // 18
		OpCodes::Push, 4,       // 20
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

