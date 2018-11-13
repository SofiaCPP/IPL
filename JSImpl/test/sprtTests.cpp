#include <gtest/gtest.h>

#include <spasm.hpp>
#include <sstream>

using Spasm::OpCodes;

struct SPRTTest : public ::testing::Test
{
	void Run(Spasm::byte bytecode[], size_t size)
	{
		VM.Initialize(size, bytecode, Input, Output);
		ASSERT_EQ(Spasm::Spasm::RunResult::Success, VM.run());
	}
	Spasm::Spasm VM;
	std::istringstream Input;
	std::ostringstream Output;
};

TEST_F(SPRTTest, Empty)
{
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
		OpCodes::Print, 3,
	};

	Run(bytecode, sizeof(bytecode));
	ASSERT_EQ(Output.str(), "1");
}
