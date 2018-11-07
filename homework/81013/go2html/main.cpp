#include <iostream>
#include <string>
#include <fstream>
#include <sstream>

#include "CommonTypes.h"
#include "Lexer.h"
#include "Highlighter.h"

int main(int argc, char **argv)
{
	// todo: WRITE PROPER TESTS
	std::ifstream infile(argv[1]);
	std::ofstream offile(argv[2]);

	Highlight(infile, offile);

    return 0;
}
