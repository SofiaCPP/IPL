#include "Lexer.h"
#include "Parser.h"
#include "ASTPrinter.h"
#include "ASTInterpreter.h"
#include "ByteCodeGenerator.h"
#include <iostream>
#include <cstring>

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

class InterpreterPrinter : public ASTInterpreter::Printer
{
public:
	virtual void PrintVariable(const char* name, double value) override
	{
		std::cout << name << ": " << value << std::endl;
	}
};

void InteractiveInterpreter()
{
	char command[1000];
	ASTInterpreter i;
	InterpreterPrinter p;
	do
	{
		std::cin.getline(command, 1000);
		if (std::strcmp(command, "exit") == 0)
		{
			break;
		}
		if (std::strcmp(command, "print") == 0)
		{
			i.Print(p);
			continue;
		}
		IPLVector<Token> tokens;
		Tokenize(command, tokens);
		auto result = i.Run(Parse(tokens).get());
		while (!result.empty()) {
			std::cout << result.back() << std::endl;
			result.pop_back();
		}
	} while (true);
}

void Generate()
{
	IPLVector<Token> tokens;
	//Tokenize("var a = 0;  var b = a + 123;a+b;", tokens);
	Tokenize("var a; if(a < 4) { a = 8;} else { a = 2} ", tokens);

	std::cout << GenerateByteCode(Parse(tokens));
}

int main()
{
	//RunParseCalc();
	//InteractiveInterpreter();
	Generate();
#if defined(_WIN32)
	std::system("pause");
#endif
	return 0;
}
