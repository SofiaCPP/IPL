#include "../../../JSImpl/src/Lexer.h"
#include <Windows.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <cassert>

// Theme
#define IDENTIFIER_COLOR "#e2c071"
#define SINGLE_KEYWORD_COLOR "#b9b8b9"
#define KEYWORD_COLOR "#bd6fd6"
#define OPERATOR_KEYWORD_COLOR "#5faee8"
#define NUMBER_COLOR "#e2c071"
#define STRING_COLOR "#8daf56"
#define BACKGROUND_COLOR "#1e1e1e"
#define COMMENT_COLOR "#5d6371"
#define MISC_COLOR "#dd7076"
#define BOLD "font-weight: bold;"

void FormatCode(const IPLString& in, const IPLString& out);

IPLString GetSolutionDir();
IPLString GetInputCode(const IPLString& inputName);
bool DumpFormattedCode(const IPLVector<Token>& tokens, const IPLString& outName);
void PreviewFormattedCode(const IPLString& outName);

void DumpFileBeginning(std::ofstream& out);
void DumpTokens(std::ofstream& out, const IPLVector<Token>& tokens);
void DumpFileEnd(std::ofstream& out);

inline void DumpClassWithValue(std::ofstream& out, const IPLString& cssClass, const IPLString& lexeme);
inline void DumpClassWithValue(std::ofstream& out, const IPLString& cssClass, double number);
inline void DumpClassWithFormattedLexeme(std::ofstream& out, const IPLString& cssClass, const IPLString& lexeme);
void DumpCompatibleHTML(std::ofstream& out, const IPLString& text);

inline void DumpOperatorKeyword(std::ofstream& out, const Token& token);
inline void DumpSingleKeyword(std::ofstream& out, const Token& token);
inline void DumpKeyword(std::ofstream& out, const Token& token);
inline void DumpString(std::ofstream& out, const Token& token);
inline void DumpMisc(std::ofstream& out, const Token& token);
inline void DumpNumber(std::ofstream& out, const Token& token);
inline void DumpIdentifier(std::ofstream& out, const Token& token);
inline void DumpComment(std::ofstream& out, const Token& token);

int main()
{
	const IPLVector<IPLString> inputs = { "input1.js","input2.js", "input3.js", "input4.js"};
	const IPLString outputFilePrefix = "output";

	for (std::size_t i = 0; i < inputs.size(); ++i)
	{
		FormatCode(inputs[i], outputFilePrefix + std::to_string(i + 1) + ".html");
	}

	return 0;
}

IPLString GetSolutionDir()
{
	TCHAR* buffer = new TCHAR[MAX_PATH];
	int nSize = GetModuleFileName(NULL, buffer, MAX_PATH);

	if (nSize != 0)
	{
		IPLString ret(buffer);
		delete[] buffer;

		auto dirDelim = ret.rfind("\\bin");

		if (dirDelim != IPLString::npos)
		{
			ret.resize(dirDelim + 1);
			return ret;
		}
	}

	return IPLString();
}

IPLString GetInputCode(const IPLString& inputName)
{
	auto dir = GetSolutionDir();
	if (!dir.empty())
	{
		std::ifstream input(dir + inputName);
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

void DumpFileBeginning(std::ofstream& out)
{
	out << "<html>\n\n"
		<< "<head>\n"
		<< "\t<style>\n"
		<< "\t\tdiv {\n\t\t\twhite-space: pre-wrap;\n\t\t}\n\n"
		<< "\t\tbody {\n"
		<< "\t\t\tbackground-color: " << BACKGROUND_COLOR << ";\n"
		<< "\t\t\tfont-size: 18px;\n"
		<< "\t\t\tfont-family: 'Consolas';\n"
		<< "\t\t}\n\n"
		<< "\t\t.identifier {\n\t\t\tcolor: " << IDENTIFIER_COLOR << ";\n\t\t}\n\n"
		<< "\t\t.single-keyword {\n\t\t\tcolor: " << SINGLE_KEYWORD_COLOR << ";\n\t\t}\n\n"
		<< "\t\t.keyword {\n \t\t\tcolor: " << KEYWORD_COLOR << ";\n\t\t}\n\n"
		<< "\t\t.operator-keyword {\n\t\t\tcolor: " << OPERATOR_KEYWORD_COLOR << "; " << BOLD << "\n\t\t}\n\n"
		<< "\t\t.number {\n\t\t\tcolor: " << NUMBER_COLOR << ";\n\t\t}\n\n"
		<< "\t\t.string {\n\t\t\tcolor: " << STRING_COLOR << ";\n\t\t}\n\n"
		<< "\t\t.comment {\n\t\t\tcolor: " << COMMENT_COLOR << ";\n\t\t}\n\n"
		<< "\t\t.misc {\n\t\t\tcolor: " << MISC_COLOR << ";\n\t\t}\n"
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

void DumpCompatibleHTML(std::ofstream& out, const IPLString& text)
{
	for (auto& ch : text)
	{
		if (ch == '\n')
		{
			out << "\\n";
		}
		else if (ch == '\t')
		{
			out << "\\t";
		}
		else if (ch == '<')
		{
			out << "&#60;";
		}
		else if (ch == '>')
		{
			out << "&#62;";
		}
		else
		{
			out << ch;
		}
	}
}

inline void DumpClassWithValue(std::ofstream& out, const IPLString& cssClass, const IPLString& lexeme)
{
	out << "<span class=\"" << cssClass << "\">" << lexeme << "</span>";
}

inline void DumpClassWithValue(std::ofstream& out, const IPLString& cssClass, double number)
{
	out << "<span class=\"" << cssClass << "\">" << number << "</span>";
}

inline void DumpClassWithFormattedLexeme(std::ofstream& out, const IPLString& cssClass, const IPLString& lexeme)
{
	out << "<span class=\"" << cssClass << "\">";
	DumpCompatibleHTML(out, lexeme);
	out << "</span>";
}

inline void DumpComment(std::ofstream& out, const Token& token)
{
	DumpClassWithFormattedLexeme(out, "comment", token.Lexeme);
}

inline void DumpIdentifier(std::ofstream& out, const Token& token)
{
	DumpClassWithFormattedLexeme(out, "identifier", token.Lexeme);
}

inline void DumpNumber(std::ofstream& out, const Token& token)
{
	DumpClassWithValue(out, "number", token.Number);
}

inline void DumpMisc(std::ofstream& out, const Token& token)
{
	DumpClassWithValue(out, "misc", token.Lexeme);
}

inline void DumpString(std::ofstream& out, const Token& token)
{
	auto str = token.Lexeme;
	str.insert(str.begin(), '\\');
	str.insert(str.end() - 1, '\\');
	DumpClassWithValue(out, "string", str);
}

inline void DumpKeyword(std::ofstream& out, const Token& token)
{
	DumpClassWithValue(out, "keyword", token.Lexeme);
}

inline void DumpSingleKeyword(std::ofstream& out, const Token& token)
{
	DumpClassWithValue(out, "single-keyword", token.Lexeme);
}

inline void DumpOperatorKeyword(std::ofstream& out, const Token& token)
{
	DumpClassWithFormattedLexeme(out, "operator-keyword", token.Lexeme);
}

void DumpTokens(std::ofstream& out, const IPLVector<Token>& tokens)
{
	for (auto& token : tokens)
	{
		assert(token.Type != TokenType::Invalid);

		switch (token.Type)
		{
		case TokenType::Comment:
			DumpComment(out, token);
			break;
		case TokenType::Whitespace:
			DumpCompatibleHTML(out, token.Lexeme);
			break;
		case TokenType::Identifier:
			DumpIdentifier(out, token);
			break;
		case TokenType::Null:
		case TokenType::Undefined:
		case TokenType::True:
		case TokenType::False:
			DumpMisc(out, token);
			break;
		case TokenType::Number:
			DumpNumber(out, token);
			break;
		case TokenType::String:
			DumpString(out, token);
			break;
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
			DumpKeyword(out, token);
			break;
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
			DumpSingleKeyword(out, token);
			break;
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
		case TokenType::MinusMinus:
		case TokenType::PlusPlus:
		case TokenType::LogicalAnd:
		case TokenType::LogicalOr:
		case TokenType::StarEqual:
		case TokenType::DivideEqual:
		case TokenType::ModuloEqual:
		case TokenType::BitwiseAndEqual:
		case TokenType::BitwiseXorEqual:
		case TokenType::BitwiseOrEqual:
		case TokenType::Greater:
		case TokenType::GreaterEqual:
		case TokenType::LessEqual:
		case TokenType::Less:
		case TokenType::RightShift:
		case TokenType::LeftShift:
		case TokenType::LeftShiftEqual:
		case TokenType::RightShiftEqual:
			DumpOperatorKeyword(out, token);
			break;
		case TokenType::Eof:
			return;
		default:
			DumpMisc(out, token);
			break;
		}
	}
}

bool DumpFormattedCode(const IPLVector<Token>& tokens, const IPLString& outName)
{
	auto dir = GetSolutionDir();
	if (!dir.empty())
	{
		std::ofstream out(dir + outName);
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

void PreviewFormattedCode(const IPLString& outName)
{
	auto dir = GetSolutionDir();
	if (!dir.empty())
	{
		auto res = system((dir + outName).c_str());
		if (res == -1)
		{
			switch (errno)
			{
			case ENOENT:
			case E2BIG:
			case ENOEXEC:
			case ENOMEM:
				std::cerr << "Unable to preview output file.\n";
			default:
				break;
			}
		}
	}
}

void FormatCode(const IPLString& in, const IPLString& out)
{
	IPLString code = GetInputCode(in);
	if (!code.empty())
	{
		IPLVector<Token> tokens;
		const auto& res = Tokenize(code.c_str(), tokens, { true, true });
		if (!res.IsSuccessful)
		{
			std::cerr << "Lexer error at row " << res.Error.Row << " column " << res.Error.Column << ": " << res.Error.What;
			return;
		}

		if (DumpFormattedCode(tokens, out))
		{
			PreviewFormattedCode(out);
		}
	}
}