#include <memory>

#include "gc.h"

class DummyGC : public GarbageCollector
{
public:
    DummyGC(unsigned gcInterval)
        : m_GCInterval(gcInterval)
    {}

    void* Allocate(size_t size) override {
        if (++m_AllocsSinceGC == m_GCInterval)
        {
            GC();
            m_AllocsSinceGC = 0;
        }
        auto memory = ::operator new(size);
        m_Allocated.insert(static_cast<Object*>(memory));
        return memory;
    }

    void SetRoot(Object** root) override { m_Root = root; }

    void VisitReference(Object*, Object** to, void*) override
    {
        auto insert = m_Visited.insert(*to);
        if (insert.second)
        {
            (*to)->VisitReferences(this, nullptr);
        }
    }

    void GC()
    {
        m_Visited.clear();
        m_Visited.insert(*m_Root);
        (*m_Root)->VisitReferences(this, nullptr);

        for (auto object: m_Allocated)
        {
            if (m_Visited.find(object) == m_Visited.end())
            {
                object->~Object();
                ::operator delete(object);
            }
        }
        m_Allocated = m_Visited;
    }

    void Shutdown() override
    {
        for (auto object: m_Allocated)
        {
            object->~Object();
            ::operator delete(object);
        }

    }

    Object** m_Root;
    std::unordered_set<Object*> m_Allocated;
    std::unordered_set<Object*> m_Visited;
    unsigned m_GCInterval;
    unsigned m_AllocsSinceGC = 0;
};

std::unique_ptr<GarbageCollector> CreateGarbageCollector(int&, const char*[])
{
    return std::unique_ptr<GarbageCollector>(new DummyGC(1024));
}
