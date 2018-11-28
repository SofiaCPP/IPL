#include <gtest/gtest.h>
#include <memory>
#include <cmath>
#include <spasm.hpp>

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
	ASSERT_EQ(42U, get_arg(PC, 1));
	ASSERT_EQ(data + 3, PC);
}

TEST(Pointer, int32)
{
	int8_t data[] = { 1, 42, 0,  3, 0, 1 };
	int8_t* PC = data;
	get_op(PC);
	ASSERT_EQ(0x0003002aU, get_arg(PC, 2));
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


TEST(NaNBox, PlainDoubles)
{
	double a = floor(rand() / RAND_MAX);
    double values[] = {
        0.0, 2.89, 3.14, 0.0 / 1.0, 1.0 / a
    };
    for (auto value: values)
    {
		Spasm::Value box(value);
        ASSERT_TRUE(box.is_double());
        ASSERT_EQ(value, box.get_double());

		Spasm::Value box2(-value);
        ASSERT_TRUE(box2.is_double());
        ASSERT_EQ(-value, box2.get_double());
    }
}

TEST(NaNBox, Border)
{
	Spasm::Value v(Spasm::ValueType::Object, nullptr);
    ASSERT_TRUE(std::isnan(v.m_value.as_double));
    ASSERT_FALSE(std::isinf(v.m_value.as_double));
}

TEST(NaNBox, IsNaN)
{
	Spasm::Value v2(std::sqrt(-1.0));
    ASSERT_TRUE(v2.is_double());
}

TEST(NaNBox, Pointers)
{
    std::vector<std::unique_ptr<int>> pointers;
    int n = 1024;
    pointers.reserve(n);
    for (int i = 0; i < n; ++i)
    {
        pointers.emplace_back(std::make_unique<int>(i));
    }

    for (auto& value: pointers)
    {
        Spasm::Value box(Spasm::ValueType::Object, value.get());
        ASSERT_FALSE(box.is_double());
        ASSERT_EQ(value.get(), box.get_pointer());
    }
}
