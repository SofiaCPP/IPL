#ifndef CYCLOMATIC_H
#define CYCLOMATIC_H

#include <cstdio>

// Given this
// is 20,000 LOC, I'll probably leave parsing
// 'C' as a course project
// #courseproject

// class Parser;

namespace ParCer {

unsigned outputWithCyclomaticComplexity(FILE *fpIn, FILE *fpOut);
  
} /* Namespace ParCer */

#endif /* CYCLOMATIC_H */
