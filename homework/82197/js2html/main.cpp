#include <fstream>
#include <iostream>
#include "Lexer.h"
#include <string>


int main(int argc, char* argv[])
{
	if (argc < 2) {
		return 1;
	}

	char* inputFilePath = argv[1];

	LexerSettings settings;
	settings.CreateCommentTokens = true;
	settings.CreateWhitespaceTokens = true;

	std::ifstream in(inputFilePath);
	std::string contents((std::istreambuf_iterator<char>(in)), 
    std::istreambuf_iterator<char>());

	const char* code = contents.c_str();

	Tokenizer tokenizer(code, settings);

	LexerResult result = tokenizer.Tokenize();

	std::string output = "<!DOCTYPE html><html><head><style>body {color: white;}</style></head><body style=\"background-color:#202020;\">";

    for (const Token& token : result.tokens)
    {
        if (token.Type == TokenType::Number) {
			output.append(std::to_string(token.Number));

			continue;
		}

		output.append(token.Lexeme);
    }

	output += "</body></html>";

	std::ofstream MyFile("output.html");

	MyFile << output;

	MyFile.close();

	return 0;
}