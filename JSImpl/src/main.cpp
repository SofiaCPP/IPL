#include "Lexer.h"
#include "Parser.h"
#include "ASTPrinter.h"
#include "ASTInterpreter.h"
#include "ByteCodeGenerator.h"
#include <iostream>
#include <cstring>

#include <sstream>
#include <iterator>

#if defined(_WIN32)
#include <Windows.h>
#endif

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
	/*Tokenize("var a; if(a < 4) { a = 8; if(a < 7) {var b = 4;}} else { a = 2} ", tokens);*/
	IPLString source =  "var i = 0; \n"
		"for (var j = 0; 10 >= j; j++)\n"
						"{\n"
						"	i = i + j;\n"
						"}";
	Tokenize(source.c_str(), tokens);

	auto asmb = GenerateByteCode(Parse(tokens), source,
		ByteCodeGeneratorOptions(ByteCodeGeneratorOptions::OptimizationsType::None, true));

	//sets the color to intense red on blue background
#if defined(_WIN32)
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	std::istringstream asmbStream(asmb);
	while (asmbStream.good())
	{
		IPLString it;
		std::getline(asmbStream, it);
		if (it[0] == 'D')
		{
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN| BACKGROUND_BLUE | FOREGROUND_INTENSITY);
			auto dd = it.find("@@=>");
			std::cout << it.substr(0, dd);
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | BACKGROUND_BLUE | FOREGROUND_INTENSITY);
			std::cout << it.substr(dd, 4);
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN | BACKGROUND_BLUE | FOREGROUND_INTENSITY);
			std::cout << it.substr(dd + 4, it.size()) << std::endl;
		}
		else
		{
			std::cout << it << std::endl;
		}
		SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
	}
#else
	std::cout << asmb;
#endif
}

int main()
{
	//RunParseCalc();
	//InteractiveInterpreter();
	Generate();
#if defined(_WIN32)
	//std::system("pause");
#endif
	return 0;
}
