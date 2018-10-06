#pragma once

#include "CommonTypes.h"
#include "Expression.h"
#include "Lexer.h"

ExpressionPtr Parse(const IPLVector<Token>&, const std::function<void()>& onError = {});
