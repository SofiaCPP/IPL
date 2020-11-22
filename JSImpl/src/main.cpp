#include "Lexer.h"
#include "Parser.h"
#include "ASTPrinter.h"
#include "ASTInterpreter.h"
#include "ByteCodeGenerator.h"
#include <iostream>
#include <cstring>
#include <unordered_map>
#include <sstream>
#include <fstream>
#include <iterator>

#if defined(_WIN32)
#include <Windows.h>
#endif

void RunParseCalc()
{
	//Tokenize("var a = 0;  var b = a + 123;a+b;", tokens);
	auto tokens = Tokenize("var a = 5; if(a == 5){a++;}").tokens;

	// TODO make actual test :D
	auto expr = Parse(tokens);
	PrintAST(expr, std::cout);
	//ASTInterpreter i;
    //std::cout << "Running" << std::endl;
	//auto result = i.Run(expr.get());
    //std::cout << "Stack after run, top to bottom:" << std::endl;
    //while (!result.empty()) {
    //    std::cout << result.back() << std::endl;
    //    result.pop_back();
    //}
	//std::cout << "S = " << i.ModifyVariable("a") << std::endl;
}

class InterpreterPrinter : public ASTInterpreter::Printer
{
public:
	virtual void PrintVariable(const char* name, double value) override
	{
		std::cout << name << ": number : " << value << std::endl;
	}
	virtual void PrintVariable(const char* name, const IPLString& value) override
	{
		std::cout << name << ": string : " << value << std::endl;
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
		auto tokens = Tokenize(command).tokens;
		auto result = i.Run(Parse(tokens).get());
		while (!result.empty()) {
			std::cout << result.back() << std::endl;
			result.pop_back();
		}
	} while (true);
}

void Generate()
{
	//Tokenize("var a = 0;  var b = a + 123;a+b;", tokens);
	/*Tokenize("var a; if(a < 4) { a = 8; if(a < 7) {var b = 4;}} else { a = 2} ", tokens);*/
	//IPLString source =  "var i = 0; \n"
	//					"for (var j = 0; j < 10; j++)\n"
	//					"{\n"
	//					"	i = i + j;\n"
	//					"}";
	IPLString source = "var a = {c: 3}; var k = a.c;\n";
	auto tokens = Tokenize(source.c_str()).tokens;

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


class CammandLineApp 
{
public:
	CammandLineApp()
		: m_PrintAst(false)
		, m_PrintTokens(false)
	{
		m_CommandMapping["--output"] = CommandType::Output;
		m_CommandMapping["--input"] = CommandType::Input;
		m_CommandMapping["--ast"] = CommandType::PrintAST;
		m_CommandMapping["--tokens"] = CommandType::PrintTokens;
		m_CommandMapping["--help"] = CommandType::Help;
	}

	enum class CommandType
	{
		Output,
		Input,
		PrintAST,
		PrintTokens,
		Help
	};

	void PrintHelp()
	{
		std::cout << "Supports following arguments:" << std::endl
			<< "--output - output file path" << std::endl
			<< "--input - input file path" << std::endl
			<< "--ast - will write ast of the input in the output" << std::endl
			<< "--tokens - will tokenize input file and will write the result as json in the output" << std::endl
			<< "--help - this message" << std::endl;
	}

	void ParseArguments(int argcount, char* arguments[])
	{
		for (int i = 1; i < argcount; )
		{
			auto it = m_CommandMapping.find(arguments[i]);
			if (it == m_CommandMapping.end())
			{
				PrintHelp();
			}
			auto command = it->second;
			++i;
			switch (command)
			{
			case CommandType::Output:
				assert(i < argcount);
				m_Output = arguments[i];
				++i;
				break;
			case CommandType::Input:
				assert(i < argcount);
				m_Input = arguments[i];
				++i;
				break;
			case CommandType::PrintAST:
				m_PrintAst = true;
				break;
			case CommandType::PrintTokens:
				m_PrintTokens = true;
				break;
			case CommandType::Help:
			default:
				PrintHelp();
				return;
			}
		}
	}

	void ExecuteCommands()
	{
		std::ifstream input(m_Input.c_str());
		input.seekg(0, std::ios::end);
		std::string code;
		int size = (int)input.tellg();
		code.resize(size);
		input.seekg(0, std::ios::beg);
		input.read(&code[0], code.size());
		input.close();

		auto tokenizationResult = Tokenize(code.c_str());
		if (!tokenizationResult.IsSuccessful)
		{
			return;
		}
		auto programAst = Parse(tokenizationResult.tokens);
		if (m_PrintAst)
		{
			std::ofstream output(m_Output, std::ofstream::trunc);
			PrintAST(programAst, output);
			output.close();
		}

		if (m_PrintTokens)
		{

		}
	}

	void RunAsCommandLine(int argcount, char* arguments[])
	{
		ParseArguments(argcount, arguments);
		ExecuteCommands();
	}
private:
	std::string m_Output;
	std::string m_Input;
	bool m_PrintAst;
	bool m_PrintTokens;
	std::unordered_map<std::string, CommandType> m_CommandMapping;
};

int main(int argc, char* argv[])
{
	(void)argv;
	(void)argc;
	// You can try =>
	// Working dir: $(ProjectDir)
	// Command arguments: --input ./../examples/code.js --output ./../examples/some.ast --ast
	//CammandLineApp cmd;
	//cmd.RunAsCommandLine(argc, argv);
	//auto r = Tokenize("a#4", {true ,true});
	//RunParseCalc();
	Generate();
#if defined(_WIN32)
	//std::system("pause");
#endif
	return 0;
}
