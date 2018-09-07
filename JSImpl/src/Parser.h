#pragma

#include "CommonTypes.h"
#include "Expression.h"
#include "Lexer.h"

ExpressionPtr Parse(IPLVector<Token>&, const std::function<void()>& onError = {});