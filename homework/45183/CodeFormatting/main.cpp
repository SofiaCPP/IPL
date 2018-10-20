#include "../../../JSImpl/src/Lexer.h"
#include <Windows.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <cassert>

#define IN_FILE "input.js"
#define OUT_FILE "output.html"

IPLString GetSolutionDir()
{
#ifdef _WIN32
	TCHAR* buffer = new TCHAR[MAX_PATH];
	int nSize = GetModuleFileName(NULL, buffer, MAX_PATH);

	if (nSize != 0)
	{
		IPLString ret(buffer);
		delete[] buffer;

		auto dirDelim = ret.rfind("\\Debug");
		if (dirDelim == IPLString::npos)
		{
			dirDelim = ret.rfind("\\Release");
		}

		if (dirDelim != IPLString::npos)
		{
			ret.resize(dirDelim + 1);
			return ret;
		}
	}
#endif // _WIN32

	return IPLString();
}
//
//bool GetInFile(const IPLString& dir, std::ifstream& in)
//{
//	IPLString path = dir + IN_FILE;
//
//	in.open(path);
//	if (in.good())
//	{
//		return true;
//	}
//
//	in.clear();
//
//	std::ofstream create(path, std::ios::app);
//	create.close();
//
//	in.open(path);
//	return in.good();
//}

IPLString GetInputCode()
{
	auto dir = GetSolutionDir();
	if (!dir.empty())
	{
		std::ifstream input(dir + IN_FILE);
		if (input.good())
		{
			std::stringstream buffer;
			buffer << input.rdbuf();
			input.close();
			return buffer.str();
		}
	}

	std::cerr << "Input file error.\n";
	return IPLString();
}

#define IDENTIFIER_COLOR "#e2c071"
#define SINGLE_KEYWORD_COLOR "#b9b8b9"
#define KEYWORD_COLOR_1 "#bd6fd6"
#define KEYWORD_COLOR_2 "#5faee8"
#define NUMBER_COLOR "#dd7076"
#define STRING_COLOR "#8daf56"
#define BACKGROUND_COLOR "#1e1e1e"

void DumpFileBeginning(std::ofstream& out)
{
	out << "<html>\n\n"
		<< "<head>\n"
		<< "\t<style>\n"
		<< "\t\tdiv {\n" << "\t\t\twhite-space: pre-wrap;\n" << "\t\t}\n\n"
		<< "\t\tbody {\n"
		<< "\t\t\tbackground-color: " << BACKGROUND_COLOR << ";\n"
		<< "\t\t\tfont-size: 18px;\n"
		<< "\t\t\tfont-family: 'Consolas';\n"
		<< "\t\t}\n\n"
		<< "\t\t.identifier {\n" << "\t\t\tcolor: " << IDENTIFIER_COLOR << ";\n" << "\t\t}\n\n"
		<< "\t\t.single-keyword {\n" << "\t\t\tcolor: " << SINGLE_KEYWORD_COLOR << ";\n" << "\t\t}\n\n"
		<< "\t\t.keyword1 {\n" << " \t\t\tcolor: " << KEYWORD_COLOR_1 << ";\n" << "\t\t}\n\n"
		<< "\t\t.keyword2 {\n" << " \t\t\tcolor: " << KEYWORD_COLOR_2 << ";\n" << "\t\t}\n\n"
		<< "\t\t.number {\n" << "\t\t\tcolor: " << NUMBER_COLOR << ";\n" << "\t\t}\n\n"
		<< "\t\t.string {\n" << "\t\t\tcolor: " << STRING_COLOR << ";\n" << "\t\t}\n"
		<< "\t</style>\n"
		<< "</head>\n\n"
		<< "<body>\n"
		<< "\t<div id='text'></div>\n"
		<< "\t<script>\n"
		<< "\t\tdocument.getElementById('text').innerHTML = '";
}

void DumpFileEnd(std::ofstream& out)
{
	out << "';\n"
		<< "\t</script>\n"
		<< "</body>\n\n"
		<< "</html>";
}

void DumpTokens(std::ofstream& out, const IPLVector<Token>& tokens)
{
	for (auto& token : tokens)
	{
		assert(token.Type != TokenType::Invalid);

		switch (token.Type)
		{
		case TokenType::Whitespace:
			for (auto& ch : token.Lexeme)
			{
				if (ch == '\n')
				{
					out << "\\n";
				}
				else if (ch == '\t')
				{
					out << "\\t";
				}
				else if (ch == ' ')
				{
					out << ch;
				}
				else
				{
					assert(false);
				}
			}
			break;
		case TokenType::Identifier:
			out << "<span class=\"identifier\">" << token.Lexeme << "</span>";
			break;
		case TokenType::Null:
		case TokenType::Undefined:
		case TokenType::True:
		case TokenType::False:
		case TokenType::Number:
			out << "<span class=\"number\">" << token.Number << "</span>";
			break;
		case TokenType::String:
		{
			auto str = token.Lexeme;
			str.insert(str.begin(), '\\');
			str.insert(str.end() - 1, '\\');
			out << "<span class=\"string\">" << str << "</span>";
			break;
		}
		case TokenType::Break:
		case TokenType::Case:
		case TokenType::Catch:
		case TokenType::Class:
		case TokenType::Continue:
		case TokenType::Debugger:
		case TokenType::Default:
		case TokenType::Delete:
		case TokenType::Do:
		case TokenType::Else:
		case TokenType::Export:
		case TokenType::Extends:
		case TokenType::Finally:
		case TokenType::For:
		case TokenType::Function:
		case TokenType::If:
		case TokenType::Import:
		case TokenType::In:
		case TokenType::Instanceof:
		case TokenType::New:
		case TokenType::Return:
		case TokenType::Super:
		case TokenType::Switch:
		case TokenType::This:
		case TokenType::Throw:
		case TokenType::Try:
		case TokenType::Typeof:
		case TokenType::Void:
		case TokenType::While:
		case TokenType::With:
		case TokenType::Yield:
		case TokenType::Const:
		case TokenType::Var:
		case TokenType::Let:
		case TokenType::Dot:
			out << "<span class=\"keyword1\">" << token.Lexeme << "</span>"; break;
		case TokenType::LeftParen:
		case TokenType::RightParen:
		case TokenType::LeftBrace:
		case TokenType::RightBrace:
		case TokenType::Comma:
		case TokenType::Semicolon:
		case TokenType::QuestionMark:
		case TokenType::Colon:
		case TokenType::LeftSquareBracket:
		case TokenType::RightSquareBracket:
			out << "<span class=\"single-keyword\">" << token.Lexeme << "</span>"; break;
		case TokenType::PlusEqual:
		case TokenType::MinusEqual:
		case TokenType::Minus:
		case TokenType::Plus:
		case TokenType::Star:
		case TokenType::Division:
		case TokenType::Modulo:
		case TokenType::BitwiseNot:
		case TokenType::BitwiseAnd:
		case TokenType::BitwiseXor:
		case TokenType::BitwiseOr:
		case TokenType::Bang:
		case TokenType::BangEqual:
		case TokenType::Equal:
		case TokenType::EqualEqual:
		case TokenType::StrictEqual:
		case TokenType::StrictNotEqual:
		case TokenType::Greater:
		case TokenType::GreaterEqual:
		case TokenType::Less:
		case TokenType::LessEqual:
		case TokenType::MinusMinus:
		case TokenType::PlusPlus:
		case TokenType::LeftShift:
		case TokenType::RightShift:
		case TokenType::LogicalAnd:
		case TokenType::LogicalOr:
		case TokenType::StarEqual:
		case TokenType::DivideEqual:
		case TokenType::ModuloEqual:
		case TokenType::LeftShiftEqual:
		case TokenType::RightShiftEqual:
		case TokenType::BitwiseAndEqual:
		case TokenType::BitwiseXorEqual:
		case TokenType::BitwiseOrEqual:
			out << "<span class=\"keyword2\">" << token.Lexeme << "</span>"; break;
		case TokenType::Eof:
			return;
		default:
			out << "<span class=\"keyword2\">" << token.Lexeme << "</span>"; break;
			break;
		}
	}
}

bool DumpFormattedCode(const IPLVector<Token>& tokens)
{
	auto dir = GetSolutionDir();
	if (!dir.empty())
	{
		std::ofstream out(dir + OUT_FILE);
		if (out.good())
		{
			DumpFileBeginning(out);
			DumpTokens(out, tokens);
			DumpFileEnd(out);
			out.close();
			return true;
		}
	}

	std::cerr << "Output file error.\n";
	return false;
}

int main()
{
	IPLString code = GetInputCode();
	if (!code.empty())
	{
		IPLVector<Token> tokens;
		const auto& res = Tokenize(code.c_str(), tokens, { true });
		if (!res.IsSuccessful)
		{
			std::cerr << "Lexer error at row " << res.Error.Row << " column " << res.Error.Column << ": " << res.Error.What;
			return 0;
		}

		DumpFormattedCode(tokens);
	}
	return 0;
}