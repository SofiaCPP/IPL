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
## Theory reminders

1. [Theory](https://github.com/TUDelft-IN4303-2016/lectures/raw/master/11-garbage-collection/Garbage%20Collection.pdf)
0. [Barriers](https://www.cs.kent.ac.uk/pubs/2010/3011/content.pdf)
0. [Lua GC](http://www.inf.puc-rio.br/~roberto/talks/gc-lua.pdf)
0. [V8 GC](http://jayconrod.com/posts/55/a-tour-of-v8-garbage-collection)
0. [Generational GC](https://www.ps.uni-saarland.de/courses/gc-ws01/slides/generational_gc.pdf)
0. [Java Z GC](http://cr.openjdk.java.net/~pliden/slides/ZGC-FOSDEM-2018.pdf)
0. [LuaJIT GC](http://wiki.luajit.org/New-Garbage-Collector)

---
## Tri-colour marking

In the core of concurrent and incremental garbage collectors

- _white_ - not used, _dead_ object
- _black_ - used, _alive_ object, whose references have been marked
- _gray_ - used, _alive_ object, whose references have yet to be marked

> references are the objects that are referenced by the current object

---
### Invariants

- Weak:

  > All white objects pointed to by a black object are reachable from some grey
  > object through a chain of white objects.

- Strong:

  > There are no pointers from black objects to white objects.

---
### Escapes

In the incremental or concurrent GC, while the marking phase is running, the
mutator can change the references of a black object and break the above
invariants.

---
### Barriers

Barriers are code that gets executed by the mutator every time it mutates the
heap. They will enforce that the invariants are being kept.

- read
- write
  - deletion

---
#### Read Barrier

Ensure the invariants by not allowing getting a _white_ object out of a _gray_
one

    def read(object, field):
        if is_grey(object):
            shade(object)
        return object[field]

Not used, since _read_ operations are more than _write_ operations in a program
and having a slower _read_ is worse than having a slower _write_

---
#### Write Barrier

Ensure the invariants by not allowing to set a _white_ object as a reference
inside a _black_ object

    def write(object, field, value):
        object[field] = value
        if is_black(object):
            shade(value)


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
