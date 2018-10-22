#pragma once

#include <ostream>

#include "CommonTypes.h"
#include "Lexer.h"

IPLResult Highlight(std::ifstream& in, std::ostream& out);
