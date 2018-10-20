#include "Lexer.h"
#include <utility>

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
	return c == '\'' || c == '"';
}

inline bool IsWhiteSpace(char c)
{
	return c == ' ' || c == '\t';
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

enum class State: unsigned char
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
	LexerResult Tokenize(IPLVector<Token>& tokens);

private:
	Token NextToken();

	inline Token ProduceToken(TokenType type);
	inline Token ProduceToken(TokenType type, IPLString lexeme, double number);
	inline Token ProduceErrorToken();

	IPLString ParseWhitespaces();
	double ParseNumber();
	IPLString ParseString();
	Keyword ParseKeyword();
	Identifier ParseIdentifier();

	inline bool IsStateSuccess() const;
	inline bool IsStateError() const;

	void SetError(const IPLString& what);

	bool SkipWhiteSpaces();
	bool SkipNewLine();
	bool Match(const char c);

	inline void NextSymbol();
	inline void NextLine();

	unsigned m_Line;
	unsigned m_Column;
	unsigned m_Current;
	const char* m_Code;

	IPLError m_Error;
	State m_GenerationState;

	LexerSettings m_Settings;

	IPLUnorderedMap<IPLString, TokenType> m_KeyWordsTable;
};

LexerResult Tokenize(const char* code, IPLVector<Token>& tokens, const LexerSettings& settings)
{
	Tokenizer tokenizer(code, settings);

	return tokenizer.Tokenize(tokens);
}

Tokenizer::Tokenizer(const char* code, const LexerSettings& settings)
	: m_Line(0)
	, m_Column(0)
	, m_Current(0)
	, m_Code(code)
	, m_Error()
	, m_GenerationState(State::Success)
	, m_Settings(settings)
{
	m_KeyWordsTable["break"] = TokenType::Break;
	m_KeyWordsTable["case"] = TokenType::Case;
	m_KeyWordsTable["catch"] = TokenType::Catch;
	m_KeyWordsTable["class"] = TokenType::Class;
	m_KeyWordsTable["const"] = TokenType::Const;
	m_KeyWordsTable["continue"] = TokenType::Continue;
	m_KeyWordsTable["debugger"] = TokenType::Debugger;
	m_KeyWordsTable["default"] = TokenType::Default;
	m_KeyWordsTable["delete"] = TokenType::Delete;
	m_KeyWordsTable["do"] = TokenType::Do;
	m_KeyWordsTable["else"] = TokenType::Else;
	m_KeyWordsTable["export"] = TokenType::Export;
	m_KeyWordsTable["extends"] = TokenType::Extends;
	m_KeyWordsTable["finally"] = TokenType::Finally;
	m_KeyWordsTable["for"] = TokenType::For;
	m_KeyWordsTable["function"] = TokenType::Function;
	m_KeyWordsTable["if"] = TokenType::If;
	m_KeyWordsTable["import"] = TokenType::Import;
	m_KeyWordsTable["in"] = TokenType::In;
	m_KeyWordsTable["instanceof"] = TokenType::Instanceof;
	m_KeyWordsTable["new"] = TokenType::New;
	m_KeyWordsTable["return"] = TokenType::Return;
	m_KeyWordsTable["super"] = TokenType::Super;
	m_KeyWordsTable["switch"] = TokenType::Switch;
	m_KeyWordsTable["this"] = TokenType::This;
	m_KeyWordsTable["throw"] = TokenType::Throw;
	m_KeyWordsTable["try"] = TokenType::Try;
	m_KeyWordsTable["typeof"] = TokenType::Typeof;
	m_KeyWordsTable["var"] = TokenType::Var;
	m_KeyWordsTable["void"] = TokenType::Void;
	m_KeyWordsTable["while"] = TokenType::While;
	m_KeyWordsTable["with"] = TokenType::With;
	m_KeyWordsTable["yield"] = TokenType::Yield;
	m_KeyWordsTable["null"] = TokenType::Null;
	m_KeyWordsTable["undefined"] = TokenType::Undefined;
	m_KeyWordsTable["true"] = TokenType::True;
	m_KeyWordsTable["false"] = TokenType::False;
}

LexerResult Tokenizer::Tokenize(IPLVector<Token>& tokens)
{
	do
	{
		const auto& token = NextToken();

		if (m_GenerationState == State::Error)
		{
			return LexerResult{ false, IPLError(m_Error) };
		}

		tokens.emplace_back(token);

	} while (tokens.back().Type != TokenType::Eof);

	return LexerResult{ true, IPLError() };
}

bool Tokenizer::SkipWhiteSpaces()
{
	const auto old = m_Current;
	while (IsWhiteSpace(m_Code[m_Current]))
	{
		NextSymbol();
	}
	return old != m_Current;
}

bool Tokenizer::SkipNewLine()
{
	const auto old = m_Current;
	while (IsNewLine(m_Code[m_Current]))
	{
		NextLine();
	}
	return old != m_Current;
}

bool Tokenizer::Match(const char c)
{
	if (c == m_Code[m_Current])
	{
		if (!IsNewLine(m_Code[m_Current]))
		{
			NextSymbol();
		}
		else
		{
			NextLine();
		}

		return true;
	}
	return false;
}

inline void Tokenizer::NextSymbol()
{
	++m_Current;
	++m_Column;
}

inline void Tokenizer::NextLine()
{
	++m_Current;
	++m_Line;
	m_Column = 0;
}

inline Token Tokenizer::ProduceToken(TokenType type)
{
	return Token{ type, m_Line, "", 0.0 };
}

inline Token Tokenizer::ProduceToken(TokenType type, IPLString lexeme, double number = 0.0)
{
	return Token{ type, m_Line, lexeme, number };
}

inline Token Tokenizer::ProduceErrorToken()
{
	return Token{ TokenType::Eof, m_Line, "", 0.0 };
}

IPLString Tokenizer::ParseWhitespaces()
{
	if (!IsWhiteSpace(m_Code[m_Current]) && !IsNewLine(m_Code[m_Current]))
	{
		RETURN_FAIL(IPLString());
	}

	auto start = m_Current;
	while (1)
	{
		if (IsWhiteSpace(m_Code[m_Current]))
		{
			NextSymbol();
		}
		else if (IsNewLine(m_Code[m_Current]))
		{
			NextLine();
		}
		else
		{
			break;
		}
	}

	return IPLString(m_Code + start, m_Code + m_Current);
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

	if (IsEnd(m_Code[m_Current])  || IsNewLine(m_Code[m_Current]))
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
		m_Current = start;
		m_Column = start;

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

void Tokenizer::SetError(const IPLString & what)
{
	m_Error = IPLError{ m_Line, m_Column, "", what };
}

Token Tokenizer::NextToken()
{
	if (IsEnd(m_Code[m_Current]))
	{
		ProduceToken(TokenType::Eof);
	}

	const auto& spaces = ParseWhitespaces();
	if (IsStateSuccess() && m_Settings.CreateWhitespaceTokens)
	{
		return ProduceToken(TokenType::Whitespace, spaces);
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
		return ProduceToken(TokenType::String, string);
	}
	else if (IsStateError())
	{
		return ProduceErrorToken();
	}

	const auto& keyword = ParseKeyword();
	if (IsStateSuccess())
	{
		return ProduceToken(keyword.Type, keyword.Content);
	}

	const auto& identifier = ParseIdentifier();
	if (IsStateSuccess())
	{
		return ProduceToken(identifier.Type, identifier.Content);
	}

	return ProduceErrorToken();
}
