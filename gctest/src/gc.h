#pragma once

#include <cstddef>
#include <unordered_set>

class ObjectVisitor;

class Object
{
public:
    virtual ~Object() {}
    virtual void VisitReferences(ObjectVisitor* visitor, void* state) = 0;
};

class ObjectVisitor
{
public:
    virtual ~ObjectVisitor() {}

    // Object** to allows the reference to be moved
    virtual void VisitReference(Object* from, Object** to, void* state) = 0;
};

class GarbageCollector : public ObjectVisitor
{
public:
    virtual void* Allocate(size_t size) = 0;

    // Object** to allows the root to be moved
    virtual void SetRoot(Object** root) = 0;

    virtual void Shutdown() = 0;
};

class DestroyVisitor : public ObjectVisitor
{
public:
    virtual void Destroy(Object* o)
    {
        auto insert_result = m_Visited.insert(o);
        if (insert_result.second)
        {
            o->VisitReferences(this, nullptr);
            o->~Object();
        }
    }

    void VisitReference(Object*, Object** to, void*) override
    {
        if (*to)
        {
            Destroy(*to);
        }
    }
    std::unordered_set<Object*> m_Visited;
};

std::unique_ptr<GarbageCollector> CreateGarbageCollector(int argc,
                                                         char* argv[]);

