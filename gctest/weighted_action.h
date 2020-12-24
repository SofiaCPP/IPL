#pragma once
#include <functional>
#include <random>
#include <vector>

template <typename Result, typename Generator>
class WeightedActionTable
{
public:
    WeightedActionTable(Generator* generator) : m_Generator(generator) {}

    typedef std::function<Result()> ActionCallback;

    void AddAction(ActionCallback action, unsigned weight)
    {
        m_Actions.emplace_back(action);
        m_Weights.emplace_back(weight);
        m_Distribution =
            std::discrete_distribution(m_Weights.begin(), m_Weights.end());
    }

    Result RunRandomAction()
    {
        auto random = m_Distribution(*m_Generator);
        return m_Actions[random]();
    }

private:
    std::vector<ActionCallback> m_Actions;
    std::vector<unsigned> m_Weights;
    Generator* m_Generator;
    std::discrete_distribution<int> m_Distribution;
};
