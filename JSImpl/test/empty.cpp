#include <gtest/gtest.h>
#include <memory>
#include <cmath>

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

/*
 * Value will be either a double, undefined, null, string, array, object,
 * function, boolean.
 *
 * Pointer values (like string, array, object, function) can be shared pointers
 * via intrusive pointer implementation. This way we can get a reference
 * counting garbage collection.
 * 
 */
struct Value
{
    Value(double v)
    {
        m_value.as_double = v;
    }

    Value(int tag, void* pointer)
    {
        assert(tag && "Zero tag is plain double");
        assert(!((size_t)pointer >> 48) && "Use only pointers to the heap.");
        m_value.as_pointer.pointer = (size_t)pointer;
        m_value.as_pointer.tag = tag;
        m_value.as_pointer.nan = 0x1fff;
    }
    struct NanPointer
    {
        size_t pointer:48;
        size_t tag:3;
        size_t nan:13;
    };

    struct CheckType
    {
        size_t payload: 48;
        size_t check:16;
    };
    union {
        double as_double;
        NanPointer as_pointer;
        CheckType to_check;

    } m_value;

    static_assert(sizeof(size_t) == 8, "unsupported arch");
    static_assert(sizeof(double) == 8, "unsupported arch");

    bool is_double() const
    {
        return m_value.to_check.check <= 0xFFF8;
    }

    double get_double() const
    {
        assert(is_double());
        return m_value.as_double;
    }

    int get_tag() {
        assert(!is_double());
        return m_value.as_pointer.tag;
    }

    void* get_pointer() const {
        return (void*)(size_t) m_value.as_pointer.pointer;
    }
};


TEST(NaNBox, PlainDoubles)
{
	double a = floor(rand() / RAND_MAX);
    double values[] = {
        0.0, 2.89, 3.14, 0.0 / 1.0, 1.0 / a
    };
    for (auto value: values)
    {
        Value box(value);
        ASSERT_TRUE(box.is_double());
        ASSERT_EQ(value, box.get_double());

        Value box2(-value);
        ASSERT_TRUE(box2.is_double());
        ASSERT_EQ(-value, box2.get_double());
    }
}

TEST(NaNBox, Border)
{
    Value v(2, nullptr);
    ASSERT_TRUE(std::isnan(v.m_value.as_double));
    ASSERT_FALSE(std::isinf(v.m_value.as_double));
}

TEST(NaNBox, IsNaN)
{
    Value v2(std::sqrt(-1.0));
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
        Value box(1, value.get());
        ASSERT_FALSE(box.is_double());
        ASSERT_EQ(value.get(), box.get_pointer());
    }
}
