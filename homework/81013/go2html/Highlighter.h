#pragma once

#include <fstream>

#include "CommonTypes.h"
#include "Lexer.h"

IPLResult Highlight(std::ifstream& in, std::ofstream& out);
