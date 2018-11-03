#include "Lexer.h"
#include "Parser.h"
#include "ASTPrinter.h"
#include "ASTInterpreter.h"
#include <iostream>

void RunParseCalc()
{
	IPLVector<Token> tokens;
	//Tokenize("var a = 0;  var b = a + 123;a+b;", tokens);
	Tokenize("var a = 5; if(a == 5){a++;}", tokens);

	// TODO make actual test :D
	auto expr = Parse(tokens);
	//ASTPrinter p(std::cout);
	//expr->Accept(p);
	ASTInterpreter i;
    std::cout << "Running" << std::endl;
	auto result = i.Run(expr.get());
    std::cout << "Stack after run, top to bottom:" << std::endl;
    while (!result.empty()) {
        std::cout << result.back() << std::endl;
        result.pop_back();
    }
	std::cout << "S = " << i.ModifyVariable("a") << std::endl;
}

int main()
{
	RunParseCalc();

#if defined(_WIN32)
	std::system("pause");
#endif
	return 0;
}
