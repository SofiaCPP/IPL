#include <gtest/gtest.h>

TEST(Empty, Empty)
{
}

int get_op(int8_t *& PC) {
	return *(PC++);
}
size_t get_arg(int8_t*& PC, int size) {
	switch (size) {
	case 0:
		return *(PC++);
	case 1:
		return *(((unsigned short*&)PC)++);
	case 2:
		return *(((unsigned int*&)PC)++);
	case 3:
		return *(((size_t*&)PC)++);
	}
	return 0;
}


TEST(Pointer, byte)
{
	int8_t data[] = { 42, 0 };
	int8_t* PC = data;
	ASSERT_EQ(42, get_op(PC));
	ASSERT_EQ(data + 1, PC);
}

TEST(Pointer, int16)
{
	int8_t data[] = { 1, 42, 0,  3};
	int8_t* PC = data;
	get_op(PC);
	ASSERT_EQ(42, get_arg(PC, 1));
	ASSERT_EQ(data + 3, PC);
}

TEST(Pointer, int32)
{
	int8_t data[] = { 1, 42, 0,  3, 0, 1 };
	int8_t* PC = data;
	get_op(PC);
	ASSERT_EQ(0x0003002a, get_arg(PC, 2));
	ASSERT_EQ(data + 5, PC);
}


TEST(Pointer, int64)
{
	int8_t data[] = { 1, 42, 0,  3, 0, 1,  0, 0, 0 };
	int8_t* PC = data;
	get_op(PC);
	ASSERT_EQ(0x010003002aU, get_arg(PC, 3));
	ASSERT_EQ(data + 9, PC);
}



