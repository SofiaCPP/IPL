#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>

template<typename T>
using IPLVector = std::vector<T>;
using IPLString = std::string;

template <typename Key, typename T>
using IPLUnorderedMap = std::unordered_map<Key, T>;

template <typename T>
using IPLSharedPtr = std::shared_ptr<T>;

template <typename T, class... Args>
inline IPLSharedPtr<T> IPLMakeSharePtr(Args&&... args) {
	return std::make_shared<T, Args...>(args...);
};

template< typename T>
using IPLEnableShared = std::enable_shared_from_this<T>;

struct IPLError
{
	unsigned Row;
	unsigned Column;
	IPLString File;
	IPLString What;
};