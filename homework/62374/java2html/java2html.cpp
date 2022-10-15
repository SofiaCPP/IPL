#include <fstream>
#include <iostream>
#include "Lexer.h"
#include <utility>
#include <string>

namespace
{
inline bool IsUpperCase(char c)
{
	return c >= 'A' && c <= 'Z';
}

inline bool IsLowerCase(char c)
{
	return c >= 'a' && c <= 'z';
}

inline bool IsDigit(char c)
{
	return c >= '0' && c <= '9';
}

inline bool IsValidIdentifierStartingChar(char c)
{
	return IsUpperCase(c) || IsLowerCase(c) || c == '_';
}

inline bool IsValidIdentifierChar(char c)
{
	return IsValidIdentifierStartingChar(c) || IsDigit(c);
}

inline bool IsStringBound(char c)
{
	return c == '"';
}

inline bool IsNewLine(char c)
{
	return c == '\n';
}

inline bool IsEnd(char c)
{
	return c == '\0';
}
}

struct Keyword
{
	TokenType Type;
	IPLString Content;
};

using Identifier = Keyword;

enum class State : unsigned char
{
	Success,
	Fail,
	Error
};

#define RETURN_SUCCESS(ret) m_GenerationState = State::Success; return ret;
#define RETURN_FAIL(ret) m_GenerationState = State::Fail; return ret;
#define RETURN_ERROR(ret) m_GenerationState = State::Error; return ret;

class Tokenizer
{
public:
	Tokenizer(const char* code, const LexerSettings& settings);
	LexerResult Tokenize();

private:
	Token NextToken();

	inline Token ProduceSkipToken();
	inline Token ProduceToken(TokenType type);
	inline Token ProduceCommentToken(TokenType type, const IPLString& comment);
	inline Token ProduceSingleCharacterToken(TokenType type, const IPLString& operatorLexeme);
	inline Token ProduceStringToken(TokenType type, const IPLString& str);
	inline Token ProduceIdentifierToken(TokenType type, const IPLString& identifier);
	inline Token ProduceKeywordToken(TokenType type, const IPLString& keyword);
	inline Token ProduceToken(TokenType type, const IPLString& lexeme, double number);
	inline Token ProduceErrorToken();
	inline Token ProduceInvalidToken();

	bool FilterToken(TokenType type);

	IPLString ParseComment();
	double ParseNumber();
	IPLString ParseString();
	Keyword ParseKeyword();
	Identifier ParseIdentifier();

	inline bool IsStateSuccess() const;
	inline bool IsStateError() const;

	void SetError(const IPLString& what);

	bool Match(const char c);

	inline void NextSymbol();
	inline void PreviousSymbol();
	inline void NextLine();

	unsigned m_Line;
	unsigned m_Column;
	unsigned m_Current;
	unsigned m_LastTokenPozition;
	const char* m_Code;

	IPLError m_Error;
	State m_GenerationState;

	LexerSettings m_Settings;

	IPLUnorderedMap<IPLString, TokenType> m_KeyWordsTable;
};

LexerResult Tokenize(const char* code, const LexerSettings& settings)
{
	Tokenizer tokenizer(code, settings);
	return tokenizer.Tokenize();
}

LexerResult Tokenize(const char * code)
{
	return Tokenize(code, { false, false });
}

Tokenizer::Tokenizer(const char* code, const LexerSettings& settings)
	: m_Line(0)
	, m_Column(0)
	, m_Current(0)
	, m_LastTokenPozition(0)
	, m_Code(code)
	, m_Error()
	, m_GenerationState(State::Success)
	, m_Settings(settings)
{
	//TODO
	m_KeyWordsTable["abstract"] = TokenType::Abstract;
	m_KeyWordsTable["continue"] = TokenType::Continue;
	m_KeyWordsTable["for"] = TokenType::For;
	m_KeyWordsTable["new"] = TokenType::New;
	m_KeyWordsTable["switch"] = TokenType::Switch;
	m_KeyWordsTable["default"] = TokenType::Default;
	m_KeyWordsTable["package"] = TokenType::Package;
	m_KeyWordsTable["synchronized"] = TokenType::Synchronize;
	m_KeyWordsTable["boolean"] = TokenType::Boolean;
	m_KeyWordsTable["do"] = TokenType::Do;
	m_KeyWordsTable["if"] = TokenType::If;
	m_KeyWordsTable["private"] = TokenType::Private;
	m_KeyWordsTable["this"] = TokenType::This;
	m_KeyWordsTable["break"] = TokenType::Break;
	m_KeyWordsTable["double"] = TokenType::Double;
	m_KeyWordsTable["implements"] = TokenType::Implements;
	m_KeyWordsTable["protected"] = TokenType::Protected;
	m_KeyWordsTable["throw"] = TokenType::Throw;
	m_KeyWordsTable["byte"] = TokenType::Byte;
	m_KeyWordsTable["else"] = TokenType::Else;
	m_KeyWordsTable["import"] = TokenType::Import;
	m_KeyWordsTable["public"] = TokenType::Public;
	m_KeyWordsTable["throws"] = TokenType::Throws;
	m_KeyWordsTable["case"] = TokenType::Case;
	m_KeyWordsTable["instanceof"] = TokenType::Instanceof;
	m_KeyWordsTable["return"] = TokenType::Return;
	m_KeyWordsTable["transient"] = TokenType::Transient;
	m_KeyWordsTable["catch"] = TokenType::Catch;
	m_KeyWordsTable["extends"] = TokenType::Extends;
	m_KeyWordsTable["int"] = TokenType::Int;
	m_KeyWordsTable["short"] = TokenType::Short;
	m_KeyWordsTable["try"] = TokenType::Try;
	m_KeyWordsTable["char"] = TokenType::Char;
	m_KeyWordsTable["final"] = TokenType::Final;
	m_KeyWordsTable["interface"] = TokenType::Interface;
	m_KeyWordsTable["static"] = TokenType::Static;
	m_KeyWordsTable["void"] = TokenType::Void;
	m_KeyWordsTable["class"] = TokenType::Class;
	m_KeyWordsTable["finally"] = TokenType::Finally;
	m_KeyWordsTable["long"] = TokenType::Long;
	m_KeyWordsTable["float"] = TokenType::Float;
	m_KeyWordsTable["super"] = TokenType::Super;
	m_KeyWordsTable["while"] = TokenType::While;

}

LexerResult Tokenizer::Tokenize()
{
	LexerResult result{ true, IPLError(), IPLVector<Token>() };
	Token token;
	do
	{
		token = NextToken();

		if (m_GenerationState == State::Error )
		{
			result.IsSuccessful = false;
			result.Error = IPLError(m_Error);
			return result;
		}
		if (FilterToken(token.Type))
		{
			result.tokens.emplace_back(token);
		}
	} while (token.Type != TokenType::Eof  && token.Type != TokenType::Invalid);

	return result;
}

Token Tokenizer::NextToken()
{
	if (IsEnd(m_Code[m_Current]))
	{
		return ProduceToken(TokenType::Eof);
	}

	const auto& comment = ParseComment();
	if (IsStateError())
	{
		return ProduceErrorToken();
	}
	if (IsStateSuccess())
	{
		return ProduceCommentToken(TokenType::Comment, comment);
	}

	// Single Char
	switch (m_Code[m_Current])
	{
	case '(': NextSymbol(); return ProduceToken(TokenType::LeftParen);
	case ')': NextSymbol(); return ProduceToken(TokenType::RightParen);
	case '{': NextSymbol(); return ProduceToken(TokenType::LeftBrace);
	case '}': NextSymbol(); return ProduceToken(TokenType::RightBrace);
	case ',': NextSymbol(); return ProduceToken(TokenType::Comma);
	case '.': NextSymbol(); return ProduceToken(TokenType::Dot);
	case '-': NextSymbol(); return Match('-') ? ProduceToken(TokenType::MinusMinus) : ProduceToken(TokenType::Minus);
	case '+': NextSymbol(); return Match('+') ? ProduceToken(TokenType::PlusPlus) : ProduceToken(TokenType::Plus);
	case ';': NextSymbol(); return ProduceToken(TokenType::Semicolon);
	case '*': NextSymbol(); return ProduceToken(TokenType::Star);
	case '/': NextSymbol(); return ProduceToken(TokenType::Division);
	case '%': NextSymbol(); return ProduceToken(TokenType::Modulo);
	case '~': NextSymbol(); return ProduceToken(TokenType::BitwiseNot);
	case '=': NextSymbol(); return Match('=') ? Match('=') ? ProduceToken(TokenType::StrictEqual) : ProduceToken(TokenType::EqualEqual) : ProduceToken(TokenType::Equal);
	case '!': NextSymbol(); return Match('=') ? Match('=') ? ProduceToken(TokenType::StrictNotEqual) : ProduceToken(TokenType::BangEqual) : ProduceToken(TokenType::Bang);
	case '>': NextSymbol(); return Match('=') ? ProduceToken(TokenType::GreaterEqual) : Match('>') ? ProduceToken(TokenType::RightShift) : ProduceToken(TokenType::Greater);
	case '<': NextSymbol(); return Match('=') ? ProduceToken(TokenType::LessEqual) : Match('<') ? ProduceToken(TokenType::LeftShift) : ProduceToken(TokenType::Less);
	case '&': NextSymbol(); return Match('&') ? ProduceToken(TokenType::LogicalAnd) : ProduceToken(TokenType::BitwiseAnd);
	case '^': NextSymbol(); return ProduceToken(TokenType::BitwiseXor);
	case '|': NextSymbol(); return Match('|') ? ProduceToken(TokenType::LogicalOr) : ProduceToken(TokenType::BitwiseOr);
	case '?': NextSymbol(); return ProduceToken(TokenType::QuestionMark);
	case ':': NextSymbol(); return ProduceToken(TokenType::Colon);
	case '[': NextSymbol(); return ProduceToken(TokenType::LeftSquareBracket);
	case ']': NextSymbol(); return ProduceToken(TokenType::RightSquareBracket);
	case '\\': NextSymbol(); return ProduceToken(TokenType::Backslash);
	case '\n': NextLine(); return ProduceToken(TokenType::NewLine);
	case ' ': NextSymbol(); return ProduceToken(TokenType::Whitespace);
	case '\t': NextSymbol(); return ProduceToken(TokenType::Tab);
	default:
		break;
	}

	const auto& number = ParseNumber();
	if (IsStateSuccess())
	{
		return ProduceToken(TokenType::Number, "", number);
	}

	const auto& string = ParseString();
	if (IsStateSuccess())
	{
		return ProduceStringToken(TokenType::String, string);
	}
	else if (IsStateError())
	{
		return ProduceErrorToken();
	}

	const auto& keyword = ParseKeyword();
	if (IsStateSuccess())
	{
		return ProduceKeywordToken(keyword.Type, keyword.Content);
	}

	const auto& identifier = ParseIdentifier();
	if (IsStateSuccess())
	{
		return ProduceIdentifierToken(identifier.Type, identifier.Content);
	}

	return ProduceInvalidToken();
}

bool Tokenizer::FilterToken(TokenType type)
{
	if (type == TokenType::Comment)
	{
		return m_Settings.CreateCommentTokens;
	}
	else if (type == TokenType::Whitespace ||
		type == TokenType::Tab ||
		type == TokenType::NewLine)
	{
		return m_Settings.CreateWhitespaceTokens;
	}
	return true;
}

bool Tokenizer::Match(const char c)
{
	if (c == m_Code[m_Current])
	{
		NextSymbol();
		return true;
	}
	return false;
}

inline void Tokenizer::NextSymbol()
{
	++m_Current;
	++m_Column;
}

inline void Tokenizer::PreviousSymbol()
{
	--m_Current;
	--m_Column;
}

inline void Tokenizer::NextLine()
{
	++m_Current;
	++m_Line;
	m_LastTokenPozition = m_Column = 0;
}

inline Token Tokenizer::ProduceIdentifierToken(TokenType type, const IPLString& identifier) {
	auto token = Token { type, m_Line, m_LastTokenPozition, "<p style=\"display:inline; color:red\">" + identifier + "</p>", 0.0};

	m_LastTokenPozition = m_Column;

	return token;
}

inline Token Tokenizer::ProduceKeywordToken(TokenType type, const IPLString& keyword) {
	auto token = Token { type, m_Line, m_LastTokenPozition, "<p style=\"display:inline; color:brown\">" + keyword + "</p>", 0.0};

	m_LastTokenPozition = m_Column;

	return token;
}

inline Token Tokenizer::ProduceStringToken(TokenType type, const IPLString& str) {
	auto token = Token { type, m_Line, m_LastTokenPozition, "<p style=\"display:inline; color:purple\">" + str + "</p>", 0.0};

	m_LastTokenPozition = m_Column;

	return token;
}

inline Token Tokenizer::ProduceCommentToken(TokenType type, const IPLString& comment) {
	auto token = Token { type, m_Line, m_LastTokenPozition, "<p style=\"display:inline; color:green\">" + comment + "</p>", 0.0};

	m_LastTokenPozition = m_Column;

	return token;
}

inline Token Tokenizer::ProduceToken(TokenType type, const IPLString& lexeme, double number = 0.0)
{
	auto token = Token{ type, m_Line, m_LastTokenPozition, lexeme, number };
	m_LastTokenPozition = m_Column;
	return token;
}

inline Token Tokenizer::ProduceSkipToken()
{
	return ProduceToken(TokenType::Invalid, "");
}

inline Token Tokenizer::ProduceSingleCharacterToken(TokenType type, const IPLString& operatorLexeme) {
	auto token = Token { type, m_Line, m_LastTokenPozition, "<p style=\"display:inline; color:blue\">" + operatorLexeme + "</p>", 0.0};

	m_LastTokenPozition = m_Column;

	return token;
}

inline Token Tokenizer::ProduceToken(TokenType type)
{
	switch (type)
	{
	case TokenType::RightSquareBracket:
		return ProduceSingleCharacterToken(type, "]");
	case TokenType::LeftSquareBracket:
		return ProduceSingleCharacterToken(type, "[");
	case TokenType::Colon:
		return ProduceSingleCharacterToken(type, ":");
	case TokenType::QuestionMark:
		return ProduceSingleCharacterToken(type, "?");
	case TokenType::BitwiseXor:
		return ProduceSingleCharacterToken(type, "^");
	case TokenType::LogicalOr:
		return ProduceSingleCharacterToken(type, "||");
	case TokenType::BitwiseOr:
		return ProduceSingleCharacterToken(type, "|");
	case TokenType::LogicalAnd:
		return ProduceSingleCharacterToken(type, "&&");
	case TokenType::BitwiseAnd:
		return ProduceSingleCharacterToken(type, "&");
	case TokenType::LessEqual:
		return ProduceSingleCharacterToken(type, "<=");
	case TokenType::LeftShift:
		return ProduceSingleCharacterToken(type, "<<");
	case TokenType::Less:
		return ProduceSingleCharacterToken(type, "<");
	case TokenType::GreaterEqual:
		return ProduceSingleCharacterToken(type, ">=");
	case TokenType::RightShift:
		return ProduceSingleCharacterToken(type, ">>");
	case TokenType::Greater:
		return ProduceSingleCharacterToken(type, ">");
	case TokenType::StrictNotEqual:
		return ProduceSingleCharacterToken(type, "!==");
	case TokenType::BangEqual:
		return ProduceSingleCharacterToken(type, "!=");
	case TokenType::Bang:
		return ProduceSingleCharacterToken(type, "!");
	case TokenType::StrictEqual:
		return ProduceSingleCharacterToken(type, "===");
	case TokenType::EqualEqual:
		return ProduceSingleCharacterToken(type, "==");
	case TokenType::Equal:
		return ProduceSingleCharacterToken(type, "=");
	case TokenType::BitwiseNot:
		return ProduceSingleCharacterToken(type, "~");
	case TokenType::Modulo:
		return ProduceSingleCharacterToken(type, "%");
	case TokenType::Division:
		return ProduceSingleCharacterToken(type, "/");
	case TokenType::LeftParen:
		return ProduceSingleCharacterToken(type, "(");
	case TokenType::RightParen:
		return ProduceSingleCharacterToken(type, ")");
	case TokenType::LeftBrace:
		return ProduceSingleCharacterToken(type, "{");
	case TokenType::RightBrace:
		return ProduceSingleCharacterToken(type, "}");
	case TokenType::Comma:
		return ProduceSingleCharacterToken(type, ",");
	case TokenType::Dot:
		return ProduceSingleCharacterToken(type, ".");
	case TokenType::Minus:
		return ProduceSingleCharacterToken(type, "-");
	case TokenType::MinusMinus:
		return ProduceSingleCharacterToken(type, "--");
	case TokenType::Plus:
		return ProduceSingleCharacterToken(type, "+");
	case TokenType::PlusPlus:
		return ProduceSingleCharacterToken(type, "++");
	case TokenType::Semicolon:
		return ProduceSingleCharacterToken(type, ";");
	case TokenType::Star:
		return ProduceSingleCharacterToken(type, "*");
	case TokenType::Backslash:
		return ProduceSingleCharacterToken(type, "\\");
	case TokenType::NewLine:
		return ProduceToken(TokenType::NewLine, "<br></br>");
	case TokenType::Whitespace: 
		return ProduceToken(TokenType::Whitespace, "&nbsp;");
	case TokenType::Tab: 
		return ProduceToken(TokenType::Tab, "&nbsp;&nbsp;&nbsp;&nbsp;");
	default: // EoF Invalid
		break;
	}
	return ProduceToken(type, "");
}

inline Token Tokenizer::ProduceErrorToken()
{
	return ProduceToken(TokenType::Invalid, "");
}

inline Token Tokenizer::ProduceInvalidToken()
{
	SetError("Invalid or unexpected token");
	RETURN_ERROR(ProduceToken(TokenType::Invalid, ""));
}

IPLString Tokenizer::ParseComment()
{
	auto start = m_Current;

	if (!Match('/'))
	{
		RETURN_FAIL(IPLString());
	}

	if (Match('/'))
	{
		while (!IsEnd(m_Code[m_Current]) && !IsNewLine(m_Code[m_Current]))
		{
			NextSymbol();
		}
		RETURN_SUCCESS(IPLString(m_Code + start, m_Code + m_Current))
	}
	else if (!Match('*'))
	{
		PreviousSymbol();
		RETURN_FAIL(IPLString());
	}

	while (!IsEnd(m_Code[m_Current]))
	{
		if (Match('*') && Match('/'))
		{
			RETURN_SUCCESS(IPLString(m_Code + start, m_Code + m_Current));
		}
		NextSymbol();
	}

	SetError("unterminated comment");
	RETURN_ERROR(IPLString());
}

double Tokenizer::ParseNumber()
{
	if (!IsDigit(m_Code[m_Current]))
	{
		RETURN_FAIL(0.0);
	}

	size_t parsedBytes = 0;
	double number = std::stod(m_Code + m_Current, &parsedBytes);
	m_Current += static_cast<unsigned>(parsedBytes);
	m_Column += static_cast<unsigned>(parsedBytes);

	RETURN_SUCCESS(number);
}

IPLString Tokenizer::ParseString()
{
	if (!IsStringBound(m_Code[m_Current]))
	{
		RETURN_FAIL(IPLString());
	}

	char bound = m_Code[m_Current];
	auto start = m_Current;

	// skip first " or '
	NextSymbol();
	while (m_Code[m_Current] != bound && !IsEnd(m_Code[m_Current]) && !IsNewLine(m_Code[m_Current]))
	{
		NextSymbol();
	}

	if (IsEnd(m_Code[m_Current]) || IsNewLine(m_Code[m_Current]))
	{
		SetError("\"\" string literal contains an unescaped line break");
		RETURN_ERROR(IPLString());
	}

	// skip second " or '
	NextSymbol();

	RETURN_SUCCESS(IPLString(m_Code + start, m_Code + m_Current));
}

Keyword Tokenizer::ParseKeyword()
{
	if (!IsLowerCase(m_Code[m_Current]))
	{
		RETURN_FAIL(Keyword());
	}

	auto start = m_Current;

	NextSymbol();
	while (IsLowerCase(m_Code[m_Current]))
	{
		NextSymbol();
	}

	auto key = IPLString(m_Code + start, m_Code + m_Current);
	auto keyword = m_KeyWordsTable.find(key);

	if (keyword == m_KeyWordsTable.end())
	{
		// It's not a keyword so we must revert current counter
		m_Column -= m_Current - start;
		m_Current = start;

		RETURN_FAIL(Keyword());
	}

	RETURN_SUCCESS((Keyword{ keyword->second, keyword->first }));
}

Identifier Tokenizer::ParseIdentifier()
{
	if (!IsValidIdentifierStartingChar(m_Code[m_Current]))
	{
		RETURN_FAIL(Identifier());
	}

	auto start = m_Current;

	NextSymbol();
	while (IsValidIdentifierChar(m_Code[m_Current]))
	{
		NextSymbol();
	}

	RETURN_SUCCESS((Identifier{ TokenType::Identifier, IPLString(m_Code + start, m_Code + m_Current) }));
}

inline bool Tokenizer::IsStateSuccess() const
{
	return m_GenerationState == State::Success;
}

inline bool Tokenizer::IsStateError() const
{
	return m_GenerationState == State::Error;
}

void Tokenizer::SetError(const IPLString& what)
{
	m_Error = IPLError{ m_Line, m_Column, "", "Syntax error: " + what };
}

int main(int argc, char* argv[])
{
	if (argc == 0) {
		return 1;
	}

	char* inputFilePath = argv[1];

	LexerSettings* settings = new LexerSettings();
	settings->CreateCommentTokens = true;
	settings->CreateWhitespaceTokens = true;

	std::ifstream in(inputFilePath);
	std::string contents((std::istreambuf_iterator<char>(in)), 
    std::istreambuf_iterator<char>());

	const char* code = contents.c_str();

	Tokenizer* tokenizer = new Tokenizer(code, *settings);

	LexerResult result = tokenizer->Tokenize();

	std::string output = "<html>";

	for (int i = 0; i < result.tokens.size(); i++) {
		Token token = result.tokens[i];

		if (token.Type == TokenType::Number) {
			output.append(std::to_string(token.Number));

			continue;
		}

		output.append(token.Lexeme);
	}

	output += "</html>";

	std::ofstream MyFile("output.html");

	MyFile << output;

	MyFile.close();

	return 0;
}
