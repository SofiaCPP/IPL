#include <memory>

#include "gc.h"

class LeakGC : public GarbageCollector
{
    void* Allocate(size_t size) override { return ::operator new(size); }

    void SetRoot(Object* root) override { m_Root = root; }

    void VisitReference(Object*, Object*, void*) override {}

    void Shutdown() override
    {
        DestroyVisitor dv;
        dv.Destroy(m_Root);
    }

    Object* m_Root;
};

std::unique_ptr<GarbageCollector> CreateGarbageCollector(int&, const char*[])
{
    return std::unique_ptr<GarbageCollector>(new LeakGC());
}
