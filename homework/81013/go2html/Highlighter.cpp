#include <iostream>
#include <fstream>

#include "CommonTypes.h"
#include "Lexer.h"
#include "Highlighter.h"

#define KEYWORD_COLOR "#ad42f4"
#define IDENTIFIER_COLOR "#41c4f4"
#define OPERATOR_COLOR "#f49541"
#define NUMBER_COLOR "#bab103"
#define STRING_COLOR "#ff11bf"
#define RUNE_COLOR "#5abc00"
#define CONST_COLOR "#f94040"
#define	COMMENT_COLOR "#0b8700"

namespace
{
	void PrintToken(
			const Token& token,
			const IPLString& className,
			std::ostream& out)
	{
		switch (token.Type)
		{
		case TokenType::Whitespace:
			out << token.Lexeme;
			break;
		case TokenType::Float:
		case TokenType::DecimalInteger:
		case TokenType::ImaginaryNum:
			out << "<span class=\"" << className << "\">" << token.Number << "</span>";
		default:
			out << "<span class=\"" << className << "\">" << token.Lexeme << "</span>";
			break;
		}
	}
}

IPLResult Highlight(std::ifstream& infile, std::ostream& out)
{
	out <<
	"<html>\n"
	"<head>\n"
	"<style>\n"
		".keyword {\n"
  			"color: " << KEYWORD_COLOR << ";\n"
		"}\n"
		"\n"
		".identifier {\n"
  			"color: " << IDENTIFIER_COLOR << ";\n"
		"}\n"
		"\n"
		".operator {\n"
  			"color: " << OPERATOR_COLOR << ";\n"
		"}\n"
		"\n"
		".number {\n"
  			"color: " << NUMBER_COLOR << ";\n"
		"}\n"
		"\n"
		".string {\n"
  			"color: " << STRING_COLOR << ";\n"
		"}\n"
		"\n"
		".rune {\n"
			"color: " << RUNE_COLOR << ";\n"
		"}\n"
		"\n"
		".const {\n"
  			"color: " << CONST_COLOR << ";\n"
		"}\n"
		"\n"
		".comment {\n"
  			"color: " << COMMENT_COLOR << ";\n"
		"}\n"
	"</style>\n"
	"</head>\n"
	"<body><pre>\n" << std::endl;

	std::string line;
	std::vector<Token> tokens;

	while (std::getline(infile, line))
	{
		IPLResult result = Tokenize(line.c_str(), tokens, {true, true});
		if (!result.IsSuccessful)
		{
			return result;
		}

		for (auto& token : tokens)
		{
			switch (token.Type)
			{
				case TokenType::Break:
				case TokenType::Default:
				case TokenType::Func:
				case TokenType::Interface:
				case TokenType::Select:
				case TokenType::Case:
				case TokenType::Defer:
				case TokenType::Go:
				case TokenType::Map:
				case TokenType::Struct:
				case TokenType::Chan:
				case TokenType::Else:
				case TokenType::Goto:
				case TokenType::Package:
				case TokenType::Switch:
				case TokenType::Const:
				case TokenType::Fallthrough:
				case TokenType::If:
				case TokenType::Range:
				case TokenType::Type:
				case TokenType::Continue:
				case TokenType::For:
				case TokenType::Import:
				case TokenType::Return:
				case TokenType::Var:

					PrintToken(token, "keyword", out);
					break;

				case TokenType::Plus:
				case TokenType::Minus:
				case TokenType::Star:
				case TokenType::Division:
				case TokenType::Modulo:

				case TokenType::NOT:
				case TokenType::BitwiseAND:
				case TokenType::BitwiseOR:
				case TokenType::BitwiseXOR:
				case TokenType::BitClear:
				case TokenType::ShiftLeft:
				case TokenType::ShiftRight:

				case TokenType::PlusEqual:
				case TokenType::MinusEqual:
				case TokenType::MultEqual:
				case TokenType::DivEqual:
				case TokenType::ModuloEqual:

				case TokenType::ConditionalAND:
				case TokenType::ConditionalOR:
				case TokenType::ReceiveOperator:
				case TokenType::PlusPlus:
				case TokenType::MinusMinus:

				case TokenType::Equal:
				case TokenType::Less:
				case TokenType::Greater:

				case TokenType::NOTEqual:
				case TokenType::LessEqual:
				case TokenType::GreaterEqual:

				case TokenType::Assignment:
				case TokenType::ShortAssignment:

				case TokenType::Variadic:

				case TokenType::LeftParen:
				case TokenType::RightParen:
				case TokenType::LeftSquareBracket:
				case TokenType::RightSquareBracket:
				case TokenType::LeftBrace:
				case TokenType::RightBrace:

				case TokenType::Comma:
				case TokenType::Dot:

				case TokenType::Semicolon:
				case TokenType::Colon:

					PrintToken(token, "operator", out);
					break;

				case TokenType::Float:
				case TokenType::DecimalInteger:
				case TokenType::ImaginaryNum:

					PrintToken(token, "number", out);
					break;

				case TokenType::String:

					PrintToken(token, "string", out);
					break;

				case TokenType::True:
				case TokenType::False:
				case TokenType::Nil:

					PrintToken(token, "const", out);
					break;

				case TokenType::Comment:

					PrintToken(token, "comment", out);
					break;

				default:
					PrintToken(token, "", out);
					break;
			}
		}

		out << std::endl;
		tokens.clear();
	}

	out <<
	"</pre></body>\n"
	"</html>" << std::endl;

	return {true, IPLError() };
}