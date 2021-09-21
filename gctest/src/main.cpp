#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstdio>
#include <filesystem>
#include <memory>
#include <random>
#include <string>
#include "weighted_action.h"

#include "minitrace/minitrace.h"

#include "gc.h"

#include "cxxopts.hpp"

typedef std::mt19937 RandomGenerator;

class RecycledObject : public Object
{
public:
    RecycledObject() { ++Alive; }

    ~RecycledObject() { --Alive; }
    static std::atomic<size_t> Alive;
    typedef Object** Location;

    virtual Location GetLocation(RandomGenerator& random) = 0;
    virtual size_t GetSize() const = 0;
    unsigned m_Generation;
};

std::atomic<size_t> RecycledObject::Alive = 0;

class LeafObject : public RecycledObject
{
public:
    LeafObject(unsigned size) : m_Size(size)
    {
        std::fill(m_Extra, m_Extra + size, uint8_t(0xab));
    }

    ~LeafObject() {}

    void VisitReferences(ObjectVisitor*, void*) override {}

    RecycledObject::Location GetLocation(RandomGenerator&) override
    {
        return nullptr;
    }

    size_t GetSize() const override { return sizeof(*this) + m_Size; }

private:
    unsigned m_Size;
    uint8_t m_Extra[1];
};

void* AllocateGCMemory(GarbageCollector* gc, size_t size)
{
    MTR_SCOPE_I("GC", "Allocate", "size", int(size));
    return gc->Allocate(size);
}

Object* CreateLeaf(GarbageCollector* gc, unsigned extraSize)
{
    auto memory = AllocateGCMemory(gc, sizeof(LeafObject) + extraSize);
    auto result = new (memory) LeafObject(extraSize);
    return result;
}

class ScannedObject : public RecycledObject
{
public:
    ScannedObject(unsigned children) : m_Size(children)
    {
        std::fill(m_Children, m_Children + m_Size, nullptr);
    }
    ~ScannedObject() {}

    void VisitReferences(ObjectVisitor* visitor, void* state) override
    {
        auto end = m_Children + m_Size;
        for (auto child = m_Children; child < end; ++child)
        {
            if (*child)
            {
                visitor->VisitReference(this, child, state);
            }
        }
    }

    RecycledObject::Location GetLocation(RandomGenerator& generator) override
    {
        std::uniform_int_distribution<unsigned> dist(0, m_Size - 1);
        return m_Children + dist(generator);
    }

    size_t GetSize() const override
    {
        return sizeof(*this) + m_Size * sizeof(Object*);
    }

private:
    unsigned m_Size;
    Object* m_Children[1];
};

Object* CreateScanned(GarbageCollector* gc, unsigned children)
{
    auto extraSize = children * sizeof(Object*);
    auto memory = AllocateGCMemory(gc, sizeof(ScannedObject) + extraSize);
    auto result = new (memory) ScannedObject(children);
    return result;
}

RecycledObject::Location GetRandomLocation(Object* root,
                                           RandomGenerator& generator)
{
    auto result = static_cast<RecycledObject*>(root)->GetLocation(generator);
    std::uniform_int_distribution<unsigned> dist(0, 99);
    auto nonleaf = result;
    while (result && *result)
    {
        if (dist(generator) < 5)
        {
            return result;
        }
        nonleaf = result;
        result = static_cast<RecycledObject*>(*result)->GetLocation(generator);
    }

    return result ? result : nonleaf;
}

template <typename Factory, typename Dist>
struct ObjectFactory
{
    ObjectFactory(Factory f,
                  Dist dist,
                  RandomGenerator* g,
                  GarbageCollector* gc)
        : m_Factory(f), m_Dist(dist), m_Generator(g), m_GC(gc)
    {
    }

    Object* operator()() const { return m_Factory(m_GC, m_Dist(*m_Generator)); }

    Factory m_Factory;
    Dist m_Dist;
    RandomGenerator* m_Generator;
    GarbageCollector* m_GC;
};

template <typename Factory, typename Dist>
ObjectFactory<Factory, Dist> MakeFactory(Factory f,
                                         Dist d,
                                         RandomGenerator* g,
                                         GarbageCollector* gc)
{
    return ObjectFactory<Factory, Dist>(f, d, g, gc);
}

template <typename Dist>
struct SizeDistribution
{
    SizeDistribution(Dist dist, unsigned min, unsigned max)
        : m_Dist(dist), m_Min(min), m_Max(max)
    {
    }

    template <typename G>
    unsigned operator()(G& g) const
    {
        auto r = unsigned(m_Dist(g));
        return (m_Min < r) ? (r < m_Max ? r : m_Max) : m_Min;
    }

    mutable Dist m_Dist;
    unsigned m_Min;
    unsigned m_Max;
};

template <typename Dist>
SizeDistribution<Dist> MakeSizeDist(Dist d, unsigned min, unsigned max)
{
    return SizeDistribution<Dist>(d, min, max);
}

typedef WeightedActionTable<Object*, RandomGenerator> CreateTable;
typedef WeightedActionTable<void, RandomGenerator> OperationsTable;

void NewObject(RecycledObject* root, CreateTable& create, RandomGenerator& rg)
{
    auto location = GetRandomLocation(root, rg);
    *location = create.RunRandomAction();
}

void NewTempObject(CreateTable& create)
{
    create.RunRandomAction();
}

void DeleteObject(RecycledObject* root, RandomGenerator& rg)
{
    auto location = GetRandomLocation(root, rg);
    *location = nullptr;
}

void CopyObject(RecycledObject* root, RandomGenerator& rg)
{
    auto from = GetRandomLocation(root, rg);
    auto to = GetRandomLocation(root, rg);
    *to = *from;
}

void MoveObject(RecycledObject* root, RandomGenerator& rg)
{
    auto from = GetRandomLocation(root, rg);
    auto to = GetRandomLocation(root, rg);
    *to = *from;
    *from = nullptr;
}

void SwapObjects(RecycledObject* root, RandomGenerator& rg)
{
    auto a = GetRandomLocation(root, rg);
    auto b = GetRandomLocation(root, rg);
    std::swap(*a, *b);
}

struct HeapStatsVisitor : ObjectVisitor
{
    void ComputeStats(Object* root)
    {
        auto recycled = static_cast<RecycledObject*>(root);
        m_CurrentGeneration = ++recycled->m_Generation;
        m_Reachable = 1;
        m_UsedMemory += recycled->GetSize();
        m_Depth = 1;
        unsigned depth = 1;
        root->VisitReferences(this, &depth);
    }
    void VisitReference(Object*, Object** to, void* state) override
    {
        auto recycled = static_cast<RecycledObject*>(*to);
        if (recycled->m_Generation < m_CurrentGeneration)
        {
            ++m_Reachable;
            m_UsedMemory += recycled->GetSize();
            recycled->m_Generation = m_CurrentGeneration;
            auto depth = *static_cast<unsigned*>(state);
            ++depth;
            if (depth > m_Depth)
            {
                m_Depth = depth;
            }
            recycled->VisitReferences(this, &depth);
        }
    }
    size_t m_Reachable = 0;
    unsigned m_Depth = 0;
    unsigned m_CurrentGeneration = 0;
    size_t m_UsedMemory = 0;
};

// extern std::atomic<size_t> TotalMemory;

CreateTable GetObjectFactory(RandomGenerator& rg, GarbageCollector* gc)
{
    CreateTable create(&rg);

    std::normal_distribution<float> u_small(64, 16);
    std::normal_distribution<float> u_medium(512, 128);
    std::normal_distribution<float> u_large(4096, 1024);

    auto size_small = MakeSizeDist(u_small, 16, 128);
    auto size_medium = MakeSizeDist(u_medium, 128, 1024);
    auto size_large = MakeSizeDist(u_large, 1024, 8192);

    create.AddAction(MakeFactory(CreateLeaf, size_small, &rg, gc), 20);
    create.AddAction(MakeFactory(CreateLeaf, size_medium, &rg, gc), 30);
    create.AddAction(MakeFactory(CreateLeaf, size_large, &rg, gc), 20);

    create.AddAction(MakeFactory(CreateScanned, size_small, &rg, gc), 20);
    create.AddAction(MakeFactory(CreateScanned, size_medium, &rg, gc), 30);
    create.AddAction(MakeFactory(CreateScanned, size_large, &rg, gc), 20);

    return create;
}

int main(int argc, char* argv[])
{
    std::string process = argv[0];
    {
        std::filesystem::path p(process);
        process = p.filename().string();
    }
    cxxopts::Options parser(process.c_str(),
                            "Test bench for garbage collectors");
    parser.add_options()
        ("s,seed", "Seed for the random generator", cxxopts::value<int>())
        ("i,initial", "Number of initial objects", cxxopts::value<unsigned>()->default_value("20000"))
        ("l,loops", "Number of loops to run", cxxopts::value<unsigned>()->default_value("2000"))
        ("o,ops", "Number of operations per iteration", cxxopts::value<unsigned>()->default_value("250"))
        ("h,help", "Print the help")
        ;
    parser.allow_unrecognised_options();

    auto options = parser.parse(argc, argv);

    if (options.count("help"))
    {
        std::printf("%s\n", parser.help().c_str());
        return 0;
    }

    mtr_init((process + ".json").c_str());
    MTR_META_PROCESS_NAME(process.c_str());
    MTR_META_THREAD_NAME("Mutator thread");

    RandomGenerator rg;
    if (options.count("seed"))
    {
        rg.seed(options["seed"].as<int>());
    }
    auto gc = CreateGarbageCollector(argc, argv);

    auto create = GetObjectFactory(rg, gc.get());

    auto initial_count = options["initial"].as<unsigned>();
    auto loops = options["loops"].as<unsigned>();
    auto operations_count = options["ops"].as<unsigned>();

    auto root = static_cast<RecycledObject*>(
        CreateScanned(gc.get(), initial_count / 10));
    gc->SetRoot(reinterpret_cast<Object**>(&root));

    OperationsTable operations(&rg);

    operations.AddAction(
        [&create, &rg, &root]() { NewObject(root, create, rg); }, 30);

    operations.AddAction([&create]() { NewTempObject(create); }, 40);

    operations.AddAction([&root, &rg]() { DeleteObject(root, rg); }, 5);

    operations.AddAction([&root, &rg]() { CopyObject(root, rg); }, 20);

    operations.AddAction([&root, &rg]() { MoveObject(root, rg); }, 10);

    operations.AddAction([&root, &rg]() { SwapObjects(root, rg); }, 20);

    {
        MTR_SCOPE("Mutator", "Initialize Heap");
        for (auto i = 0u; i < initial_count; ++i)
        {
            auto location = GetRandomLocation(root, rg);
            *location = create.RunRandomAction();
        }
    }

    for (auto l = 0u; l < loops; ++l)
    {
        {
            MTR_SCOPE_I("Mutator", "Step", "loop", l);
            for (auto o = 0u; o < operations_count; ++o)
            {
                operations.RunRandomAction();
            }
        }
        MTR_SCOPE_I("Mutator", "Walk Heap", "loop", l);
        HeapStatsVisitor stats;
        stats.ComputeStats(root);
        MTR_COUNTER("Heap", "Reachable", int(stats.m_Reachable));
        MTR_COUNTER("Heap", "Living", int(RecycledObject::Alive));
        MTR_COUNTER("Heap", "Depth", int(stats.m_Depth));
        // MTR_COUNTER("Heap", "Total KB", int(TotalMemory / 1024));
        MTR_COUNTER("Heap", "Useful KB", int(stats.m_UsedMemory / 1024));
    }

    gc->Shutdown();

    if (RecycledObject::Alive)
    {
        std::fprintf(stderr, "%zu objects leaked\n",
                     RecycledObject::Alive.load());
    }

    mtr_shutdown();
    return 0;
}
