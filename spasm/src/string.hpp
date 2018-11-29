#pragma once

#include <string>

namespace SpasmImpl
{
typedef std::string SPString;

class SPStringValue
{
   public:
    explicit SPStringValue(SPString s) : m_Value(std::move(s)) {}

    SPStringValue(const SPStringValue&) = delete;
    SPStringValue(SPStringValue&&) = default;
    SPStringValue& operator=(const SPStringValue&) = delete;
    SPStringValue& operator=(SPStringValue&&) = default;

    const SPString& GetValue() const { return m_Value; }

    bool operator==(const SPStringValue& rhs) const
    {
        return m_Value == rhs.m_Value;
    }

    bool operator==(const SPString& rhs) const { return m_Value == rhs; }

   private:
    SPString m_Value;
};
}  // namespace SpasmImpl

namespace std
{
template <>
struct hash<SpasmImpl::SPStringValue>
{
    size_t operator()(const SpasmImpl::SPStringValue& v) const
    {
        return std::hash<SpasmImpl::SPString>{}(v.GetValue());
    }
};
}  // namespace std