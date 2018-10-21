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
	LexerResult Tokenize(IPLVector<Token>& tokens);

private:
	Token NextToken();

	inline Token ProduceSkipToken();
	inline Token ProduceToken(TokenType type);
	inline Token ProduceToken(TokenType type, IPLString lexeme, double number);
	inline Token ProduceErrorToken();

	IPLString ParseComment();
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
	inline void PreviousSymbol();
	inline void NextLine();

	unsigned m_Line;
	unsigned m_Column;
	unsigned m_Current;
	const char* m_Code;

	IPLError m_Error;
	State m_GenerationState;

	LexerSettings m_Settings;
	bool m_SkipNextToken;

	IPLUnorderedMap<IPLString, TokenType> m_KeyWordsTable;
};

LexerResult Tokenize(const char* code, IPLVector<Token>& tokens, const LexerSettings& settings)
{
	Tokenizer tokenizer(code, settings);

	return tokenizer.Tokenize(tokens);
}

LexerResult Tokenize(const char * code, IPLVector<Token>& tokens)
{
	return Tokenize(code, tokens, { false, false });
}

Tokenizer::Tokenizer(const char* code, const LexerSettings& settings)
	: m_Line(0)
	, m_Column(0)
	, m_Current(0)
	, m_Code(code)
	, m_Error()
	, m_GenerationState(State::Success)
	, m_Settings(settings)
	, m_SkipNextToken(false)
{
	m_KeyWordsTable["break"] = TokenType::Break;
	m_KeyWordsTable["default"] = TokenType::Default;
	m_KeyWordsTable["func"] = TokenType::Func;
	m_KeyWordsTable["interface"] = TokenType::Interface;
	m_KeyWordsTable["select"] = TokenType::Select;
	m_KeyWordsTable["case"] = TokenType::Case;
	m_KeyWordsTable["defer"] = TokenType::Defer;
	m_KeyWordsTable["go"] = TokenType::Go;
	m_KeyWordsTable["map"] = TokenType::Map;
	m_KeyWordsTable["struct"] = TokenType::Struct;
	m_KeyWordsTable["chan"] = TokenType::Chan;
	m_KeyWordsTable["else"] = TokenType::Else;
	m_KeyWordsTable["goto"] = TokenType::Goto;
	m_KeyWordsTable["package"] = TokenType::Package;
	m_KeyWordsTable["switch"] = TokenType::Switch;
	m_KeyWordsTable["const"] = TokenType::Const;
	m_KeyWordsTable["fallthrough"] = TokenType::Fallthrough;
	m_KeyWordsTable["if"] = TokenType::If;
	m_KeyWordsTable["range"] = TokenType::Range;
	m_KeyWordsTable["type"] = TokenType::Type;
	m_KeyWordsTable["continue"] = TokenType::Continue;
	m_KeyWordsTable["for"] = TokenType::For;
	m_KeyWordsTable["import"] = TokenType::Import;
	m_KeyWordsTable["return"] = TokenType::Return;
	m_KeyWordsTable["var"] = TokenType::Var;

	m_KeyWordsTable["true"] = TokenType::True;
	m_KeyWordsTable["false"] = TokenType::False;

	m_KeyWordsTable["nil"] = TokenType::Nil;
}

LexerResult Tokenizer::Tokenize(IPLVector<Token>& tokens)
{
	do
	{
		Token token;
		do
		{
			token = NextToken();
		} while (m_SkipNextToken);

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

inline void Tokenizer::PreviousSymbol()
{
	--m_Current;
	--m_Column;
}

inline void Tokenizer::NextLine()
{
	++m_Current;
	++m_Line;
	m_Column = 0;
}

inline Token Tokenizer::ProduceToken(TokenType type, IPLString lexeme, double number = 0.0)
{
	return Token{ type, m_Line, std::move(lexeme), number };
}

inline Token Tokenizer::ProduceSkipToken()
{
	return ProduceToken(TokenType::Invalid, "");
}

inline Token Tokenizer::ProduceToken(TokenType type)
{
	switch (type)
	{
	case TokenType::LeftBrace:
		return ProduceToken(type, "{");
	case TokenType::RightBrace:
		return ProduceToken(type, "}");
	case TokenType::RightSquareBracket:
		return ProduceToken(type, "]");
	case TokenType::LeftSquareBracket:
		return ProduceToken(type, "[");
	case TokenType::LeftParen:
		return ProduceToken(type, "(");
	case TokenType::RightParen:
		return ProduceToken(type, ")");

	case TokenType::Comma:
		return ProduceToken(type, ",");
	case TokenType::Dot:
		return ProduceToken(type, ".");

	case TokenType::Plus:
		return ProduceToken(type, "+");
	case TokenType::Minus:
		return ProduceToken(type, "-");
	case TokenType::Star:
		return ProduceToken(type, "*");
	case TokenType::Division:
		return ProduceToken(type, "/");
	case TokenType::Modulo:
		return ProduceToken(type, "%");

	// todo - add rest

	default: // EoF Invalid
		break;
	}
	return ProduceToken(type, "");
}

inline Token Tokenizer::ProduceErrorToken()
{
	return ProduceToken(TokenType::Invalid, "");
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

IPLString Tokenizer::ParseWhitespaces()
{
	if (!IsWhiteSpace(m_Code[m_Current]) && !IsNewLine(m_Code[m_Current]))
	{
		RETURN_FAIL(IPLString());
	}

	auto start = m_Current;
	while (true)
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

	RETURN_SUCCESS(IPLString(m_Code + start, m_Code + m_Current));
}

// todo - many Go numbers, much refactoring
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
		m_Column  = m_Column + start - m_Current;
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

Token Tokenizer::NextToken()
{
	m_SkipNextToken = false;

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
		if (!m_Settings.CreateCommentTokens)
		{
			m_SkipNextToken = true;
			return ProduceSkipToken();
		}
		return ProduceToken(TokenType::Comment, comment);
	}

	const auto& spaces = ParseWhitespaces();
	if (IsStateSuccess())
	{
		if (!m_Settings.CreateWhitespaceTokens)
		{
			m_SkipNextToken = true;
			return ProduceSkipToken();
		}
		return ProduceToken(TokenType::Whitespace, spaces);
	}

	// Single Char
	switch (m_Code[m_Current])
	{
	case '-': NextSymbol(); return Match('-') ? ProduceToken(TokenType::MinusMinus) : ProduceToken(TokenType::Minus);
	case '+': NextSymbol(); return Match('+') ? ProduceToken(TokenType::PlusPlus) : ProduceToken(TokenType::Plus);
	case '*': NextSymbol(); return ProduceToken(TokenType::Star);
	case '/': NextSymbol(); return ProduceToken(TokenType::Division);
	case '%': NextSymbol(); return ProduceToken(TokenType::Modulo);

	case '&': NextSymbol(); return Match('&') ? ProduceToken(TokenType::BitwiseAND) : ProduceToken(TokenType::BitwiseAND);
	case '|': NextSymbol(); return Match('|') ? ProduceToken(TokenType::BitwiseOR) : ProduceToken(TokenType::BitwiseOR);
	case '^': NextSymbol(); return ProduceToken(TokenType::BitwiseXOR);

	// todo:
	// case '=': NextSymbol(); return Match('=') ? Match('=') ? ProduceToken(TokenType::StrictEqual) : ProduceToken(TokenType::EqualEqual) : ProduceToken(TokenType::Equal);
	// case '>': NextSymbol(); return Match('=') ? ProduceToken(TokenType::GreaterEqual) : Match('>') ? ProduceToken(TokenType::RightShift) : ProduceToken(TokenType::Greater);
	// case '<': NextSymbol(); return Match('=') ? ProduceToken(TokenType::LessEqual) : Match('<') ? ProduceToken(TokenType::LeftShift) : ProduceToken(TokenType::Less);
	// case '!': NextSymbol(); return Match('=') ? Match('=') ? ProduceToken(TokenType::StrictNotEqual) : ProduceToken(TokenType::BangEqual) : ProduceToken(TokenType::Bang);

	case '(': NextSymbol(); return ProduceToken(TokenType::LeftParen);
	case ')': NextSymbol(); return ProduceToken(TokenType::RightParen);
	case '[': NextSymbol(); return ProduceToken(TokenType::LeftSquareBracket);
	case ']': NextSymbol(); return ProduceToken(TokenType::RightSquareBracket);
	case '{': NextSymbol(); return ProduceToken(TokenType::LeftBrace);
	case '}': NextSymbol(); return ProduceToken(TokenType::RightBrace);

	case ',': NextSymbol(); return ProduceToken(TokenType::Comma);
	case '.': NextSymbol(); return ProduceToken(TokenType::Dot);

	case ';': NextSymbol(); return ProduceToken(TokenType::Semicolon);
	case ':': NextSymbol(); return ProduceToken(TokenType::Colon);

	// todo: see if I've left some go-specific  single chars out

	default:
		break;
	}

	const auto& number = ParseNumber();
	if (IsStateSuccess())
	{
		// todo - add integers and perhaps imaginary numbers
		return ProduceToken(TokenType::Float, "", number);
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
