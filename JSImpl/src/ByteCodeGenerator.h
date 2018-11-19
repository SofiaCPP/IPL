#pragma once

#include "Expression.h"

struct ByteCodeGeneratorOptions
{
	enum OptimizationsType
	{
		None
	};
	ByteCodeGeneratorOptions(OptimizationsType o = None, bool debug = false) : Optimisations(o), AddDebugInformation(debug) {}
	OptimizationsType Optimisations;
	bool AddDebugInformation;
};

IPLString GenerateByteCode(ExpressionPtr program, const IPLString& source, const ByteCodeGeneratorOptions& options = ByteCodeGeneratorOptions());
