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
	unsigned m_LastTokenPosition;
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
	, m_LastTokenPosition(0)
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
	m_KeyWordsTable["let"] = TokenType::Let;
	m_KeyWordsTable["void"] = TokenType::Void;
	m_KeyWordsTable["while"] = TokenType::While;
	m_KeyWordsTable["with"] = TokenType::With;
	m_KeyWordsTable["yield"] = TokenType::Yield;
	m_KeyWordsTable["null"] = TokenType::Null;
	m_KeyWordsTable["undefined"] = TokenType::Undefined;
	m_KeyWordsTable["true"] = TokenType::True;
	m_KeyWordsTable["false"] = TokenType::False;
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
	m_LastTokenPosition = m_Column = 0;
}

inline Token Tokenizer::ProduceToken(TokenType type,const IPLString& lexeme, double number = 0.0)
{
	auto token = Token{ type, m_Line, m_LastTokenPosition, lexeme, number };
	m_LastTokenPosition = m_Column;
	return token;
}

inline Token Tokenizer::ProduceSkipToken()
{
	return ProduceToken(TokenType::Invalid, "");
}

inline Token Tokenizer::ProduceToken(TokenType type)
{
	switch (type)
	{
	case TokenType::RightSquareBracket:
		return ProduceToken(type, "]");
	case TokenType::LeftSquareBracket:
		return ProduceToken(type, "[");
	case TokenType::Colon:
		return ProduceToken(type, ":");
	case TokenType::QuestionMark:
		return ProduceToken(type, "?");
	case TokenType::BitwiseXor:
		return ProduceToken(type, "^");
	case TokenType::LogicalOr:
		return ProduceToken(type, "||");
	case TokenType::BitwiseOr:
		return ProduceToken(type, "|");
	case TokenType::LogicalAnd:
		return ProduceToken(type, "&&");
	case TokenType::BitwiseAnd:
		return ProduceToken(type, "&");
	case TokenType::LessEqual:
		return ProduceToken(type, "<=");
	case TokenType::LeftShift:
		return ProduceToken(type, "<<");
	case TokenType::Less:
		return ProduceToken(type, "<");
	case TokenType::GreaterEqual:
		return ProduceToken(type, ">=");
	case TokenType::RightShift:
		return ProduceToken(type, ">>");
	case TokenType::Greater:
		return ProduceToken(type, ">");
	case TokenType::StrictNotEqual:
		return ProduceToken(type, "!==");
	case TokenType::BangEqual:
		return ProduceToken(type, "!=");
	case TokenType::Bang:
		return ProduceToken(type, "!");
	case TokenType::StrictEqual:
		return ProduceToken(type, "===");
	case TokenType::EqualEqual:
		return ProduceToken(type, "==");
	case TokenType::Equal:
		return ProduceToken(type, "=");
	case TokenType::BitwiseNot:
		return ProduceToken(type, "~");
	case TokenType::Modulo:
		return ProduceToken(type, "%");
	case TokenType::Division:
		return ProduceToken(type, "/");
	case TokenType::LeftParen:
		return ProduceToken(type, "(");
	case TokenType::RightParen:
		return ProduceToken(type, ")");
	case TokenType::LeftBrace:
		return ProduceToken(type, "{");
	case TokenType::RightBrace:
		return ProduceToken(type, "}");
	case TokenType::Comma:
		return ProduceToken(type, ",");
	case TokenType::Dot:
		return ProduceToken(type, ".");
	case TokenType::Minus:
		return ProduceToken(type, "-");
	case TokenType::MinusMinus:
		return ProduceToken(type, "--");
	case TokenType::Plus:
		return ProduceToken(type, "+");
	case TokenType::PlusPlus:
		return ProduceToken(type, "++");
	case TokenType::Semicolon:
		return ProduceToken(type, ";");
	case TokenType::Star:
		return ProduceToken(type, "*");
	case TokenType::Backslash:
		return ProduceToken(type, "\\");
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
		return ProduceToken(TokenType::Comment, comment);
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

	return ProduceInvalidToken();
}
