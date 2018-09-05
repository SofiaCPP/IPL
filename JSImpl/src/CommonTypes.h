#pragma once
#include <string>
#include <vector>
#include <unordered_map>

template<typename T>
using IPLVector = std::vector<T>;
using IPLString = std::string;

template <typename Key, typename T>
using IPLUnorderedMap = std::unordered_map<Key, T>;
