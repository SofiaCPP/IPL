---
title: "Values in the VM"
date: 2018-11-21T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---

# Values in the VM

- Languages and VMs
- NanBoxing
- NunBoxing
- Pointer compression
- Tagged arithmetic

---

# Language Semantics and VM

The VM implementation depends a lot on language semantics

- typing - static or dynamic?
- number types - integers and floating point
- strings - immutability

---

That is why it is almost every language has its own VM.

---

# Values in the VM?

    function add(a, b) {
        return a + b
    }


---

    add r1 a1 a2
    ret r1

---

- must work for numbers
- must work for strings
- must work for strings and numbers mixed
- can work for objects and arrays
    - if the target language has semantics for that

---

## How do we support that?

C++ is statically type, the value in the registers can have a single C++ type

---

## Axiom of CS

> *Every* problem can be solved by adding another level of indirection.

> ..., except too many indirections.

---

### Polymorhism

- C++ way for type erasure

- Base class with virtual methods

---

    struct Value {
        virtual ValuePtr Add(ValuePtr rhs) const = 0;
        // ...
    }

    struct NumberValue : Value {
        double m_Number;
    }

---

#### Performance

- `sizeof(NumberValue)` - 16 B - 100% overhead
    - pointer + double
- cache miss on every access to the number
- virtual call for adding two numbers

- It is so inefficient, that almost nobody does it this way

---
### What we can do?

---

    struct Value {
        bool m_IsDouble;
        union {
            double as_Double;
            void* as_Pointer;
        } m_Value;
    }

---

- still 100% memory overhead because m_Value will be aligned to 8 bytes
    - will not fit in a register
    - more cache misses

---
### Doubles and IEEE 

A double has 64-bits, from least to most significant:

1. 52 bits - mantissa
    - a leading 1 bit set to 1 is assumed, so the precision is 53 bits
2. 11 bits - exponent - from 0 to 2047
3. 1 bit - sign

Value = Mantissa * 2 ^ (exponent - 1023)

---

Not all possible values are actually valid numbers.

- Infinity - exponent is 2047, mantissa - 0


---

    double r = sqrt(-1);

 NaN - Not a Number

- sign bit is set
- exponent is all 1 (2047)
- most significant bit of the mantissa is 1
- the rest 51 bits do not matter - it is always a NaN
    - the CPU always sets them to 0


---
# The rest 51 bits do not matter

- so we can (ab)use them to store our pointers!
- but one pointer is 64 bits?

---

> For x86_64 / arm64 the addresses above 2^48 are reserved and can't be used
> from user applications

---

- 48 bits for the pointer
- 3 bits for ... type

---
### Type

    enum ValueType {
        Number = 0,
        Null = 1,
        Undefined = 2,
        Boolean = 3,
        String = 4,
        Array = 5,
        Object = 6,
        Function= 7,
    };

---
#### Why Number is 0?

- so that we treat regular NaNs as numbers.

---
#### NanBoxing


Need to split easily `double` to bits - use union to share the memory between a
`double` and structs with bitfields.

    union {
        double as_double;
        NanPointer as_pointer;
        CheckType to_check;

    } m_value;

---

    struct NanPointer
    {
        // order from least to most significant bits
        // bit field - has type size_t, but only 48 bits
        size_t pointer:48;
        size_t tag:3; // 0 for a real nan and non zero for out type
        size_t nan:13; // must be all 1s to force a nan
    };

---

    // Used to check whether the value is a double or not
    struct CheckType
    {
        size_t payload: 48;
        size_t check:16;
    };

---

    bool is_double() const
    {
        // If any of the least significant bits is non-zero,
        // then this is not a normal double or a NaN value
        return m_value.to_check.check <= 0xFFF8;
    }

    double get_double() const
    {
        assert(is_double());
        return m_value.as_double;
    }

---

    int get_tag() {
        assert(!is_double());
        return m_value.as_pointer.tag;
    }

    void* get_pointer() const {
        return (void*)(size_t) m_value.as_pointer.pointer;
    }

---

#?

---

## Strings

Strings in JavaScript are immutable, which means that changing one string
returns another.


---

#### String interning

> A method of storing only one instance of any given string, which is immutable.

This means that there is only one copy of `"answer"` inside the VM

---

Comparing strings for equality becomes very fast.

> Two strings are the same if and only if their pointers are the same.

---

    struct StringValue
    {
        std::string Value;
        // extra data
        // int RefCount;
        // int GCFlags;

    }

---
    class Spasm
    {
        typedef std::unordered_set<StringValue> StringTable;

        StringTable m_Strings;
    };

---

    Value Spasm::AllocateString(const char* s) {
        auto current = m_Strings.find(s);
        if (current != m_Strings.end())
        {
            return Value(ValueType::String, &(*current));
        }
        auto insert = m_Strings.insert(StringValue(s));
        return Value(ValueType::String, &(*insert.second));
    }


---

# ?
