#pragma once

#include "CommonTypes.h"
#include "Expression.h"
#include "Lexer.h"
#include <functional>

ExpressionPtr Parse(const IPLVector<Token>&, const std::function<void()>& onError = {});
