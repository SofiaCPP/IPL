#pragma once

#include <iostream>
#include <cassert>
#include "string.hpp"

namespace Spasm
{
enum class ValueType
{
    Number,
    Null,
    Undefined,
    Boolean,
    String,
    Array,
    Object,
    Function,
};

static_assert(int(ValueType::Function) < 8, "Too many types");

struct Value
{
    Value() { m_value.as_double = 0.0; }

    explicit Value(double v) { m_value.as_double = v; }

    explicit Value(bool v) : Value(ValueType::Boolean, (uint64_t)v) {}

    Value(ValueType tag, uint64_t payload)
    {
        assert(tag != ValueType::Number && "Zero tag is plain double");
        assert(!(payload >> 48) && "Use only pointers to the heap.");
        m_value.as_pointer.pointer = payload;
        m_value.as_pointer.tag = uint64_t(tag);
        m_value.as_pointer.nan = 0x1fff;
    }

    Value(ValueType tag, void* pointer) : Value(tag, (uint64_t)pointer) {}

    struct NanPointer
    {
        uint64_t pointer : 48;
        uint64_t tag : 3;
        uint64_t nan : 13;
    };

    struct CheckType
    {
        uint64_t payload : 48;
        uint64_t check : 16;
    };
    union {
        double as_double;
        NanPointer as_pointer;
        CheckType to_check;
        uint64_t as_int64;
    } m_value;

    static_assert(sizeof(double) == 8, "unsupported arch");

    bool is_double() const { return m_value.to_check.check <= 0xFFF8; }

    double get_double() const
    {
        assert(is_double());
        return m_value.as_double;
    }

    ValueType get_type() const
    {
        return is_double() ? ValueType::Number
                           : ValueType(m_value.as_pointer.tag);
    }

    void* get_pointer() const
    {
        return (void*)(uint64_t)m_value.as_pointer.pointer;
    }

    bool get_boolean() const
    {
        assert(get_type() == ValueType::Boolean);
        return m_value.as_pointer.pointer != 0;
    }

    explicit operator bool() const
    {
        // NaN will be true. Assume that is ok.
        return (get_type() != ValueType::Boolean && m_value.as_int64 != 0) ||
               get_boolean();
    }
};

inline std::istream& operator>>(std::istream& input, Value& value)
{
    return input >> value.m_value.as_double;
}

inline std::ostream& operator<<(std::ostream& output, const Value& value)
{
    switch (value.get_type())
    {
        case ValueType::Number:
            return output << value.m_value.as_double;
        case ValueType::Boolean:
            return output << bool(value);
        case ValueType::String:
        {
            const auto s = static_cast<const SpasmImpl::SPStringValue*>(
                value.get_pointer());
            return output << s->GetValue();
        }
        default:
            break;
    }
    return output;
}

inline Value operator+(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() + rhs.get_double());
}

inline Value operator-(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() - rhs.get_double());
}

inline Value operator*(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() * rhs.get_double());
}

inline Value operator/(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() / rhs.get_double());
}

inline Value operator%(const Value& lhs, const Value& rhs)
{
    return Value(double(int64_t(lhs.get_double()) % int64_t(rhs.get_double())));
}

inline Value operator<(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() < rhs.get_double());
}

inline Value operator>(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() > rhs.get_double());
}

inline Value operator<=(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() <= rhs.get_double());
}

inline Value operator>=(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() >= rhs.get_double());
}

inline Value operator==(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() == rhs.get_double());
}

inline Value operator!=(const Value& lhs, const Value& rhs)
{
    return Value(lhs.get_double() != rhs.get_double());
}

}  // namespace Spasm
