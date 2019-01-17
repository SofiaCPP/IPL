---
title: "Garbage collection in the VM"
date: 2018-12-05T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---
# Garbage Collection in the VM

---
## Contents

1. Allocation
2. Mark and sweep
3. Embedability
4. Incremental 
5. Optimizations

---
## Disclaimer

> This is how GC can be implemented. Everything is subject to change.

---
## Allocation

Having a custom allocator is a *must* have for most VM

- decent performance
- optimizations

---
### Allocators and C++

[Allocators in
C++](https://sofiacpp.github.io/advanced-cpp/slides/17_allocators.html)

---
### Simplicity matters

For simplicity, we are going to not use a special allocator.

---
## Mark and sweep


- mark reacheable objects starting from variables (roots)
- sweep
    - unmarked objects are returned to the free heap
    - marked objects are unmarked (for the next cycle)

---
### Mark

- every object starts as dead
- reacheable objects are marked as alive

---
#### Reachable objects


---
#### Roots

- registers
- stack
- global object / environment

Where are these stored?

---
#### Roots

- The `data_stack`
    - everything upto `m_SP`
- The global object

---
#### Marking

    void Spasm::Mark() {
        for (auto cell = &data_stack[0]; cell != m_SP; ++cell) {
            Mark(cell); // ?
        }
        Mark(m_Global);
    }

---
#### Visit every object in the heap?

- Visitor pattern again

---

    struct MarkVisitor
    {
        typedef void ResultType;

        void Visit(double d) const {}
        void VisitUndefined() const {}
        void VisitNull() const {}
        void Visit(StringValue& value) const;
        void Visit(ArrayValue& value) const;
        void Visit(ObjectValue& value) const;
        void Visit(FunctionValue& value) const;
    };

---

    void Visit(StringValue& value) const {
        if (Dead(value)) {
            SetAlive(value);
        }
    }

---

    void Visit(ArrayValue& value) const {
        if (Dead(value)) {
            SetAlive(value);
            for (auto i = 0u; i < value.length(); ++i) {
                value.item(i).Visit(*this);
            }
        }
    }

---

    void Visit(ObjectValue& value) const {
        if (Dead(value)) {
            SetAlive(value);
            for (auto* p : value.m_Properties) {
                p->first.Visit(*this);
                p->second.Visit(*this);
            }
            value.m_Prototype->Visit(*this);
        }
    }

---
#### State

Where to store whether an object is *dead* or *alive*

- `bool` per object, string, array, function
- `bitmap` that holds one bit for a range of objects
- `bit` per pointer


---
# ?
