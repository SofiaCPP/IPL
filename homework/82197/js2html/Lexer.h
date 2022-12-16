#pragma once

#include "Types.h"

enum class TokenType
{
	// Single-character tokens.
	LeftParen, // (
	RightParen, // )
	LeftBrace, // {
	RightBrace, // }
	Comma, // ,
	Dot, // .
	Minus, // -
	Plus, // +
	Semicolon, // ;
	Star, // *
	Division, // /
	Modulo, // %
	BitwiseNot, // ~
	BitwiseAnd, // &
	BitwiseXor, // ^
	BitwiseOr, // |
	QuestionMark, // ?
	Colon, // :
	LeftSquareBracket, // [
	RightSquareBracket, // ]

	// One or two character tokens
	LogicalNot, // !
	Equal, // =
	Arrow, // =>
	NotEqual, // !=
	EqualEqual, // ==
	StrictEqual, // ===
	StrictNotEqual, // !==
	Greater, // >
	GreaterEqual, // >=
	Less, // <
	LessEqual, // <=
	StarStar, // **
	MinusMinus, // --
	PlusPlus, // ++
	LeftShift, // <<
	RightShift, // >>
	UnsignedRightShift, // >>>
	LogicalAnd, // &&
	LogicalOr, // ||
	StarEqual, // *=
	StarStarEqual, // **=
	DivisionEqual, // /=
	ModuloEqual, // %=
	PlusEqual, // +=
	MinusEqual, // -=
	LeftShiftEqual, // <<=
	RightShiftEqual, // >>=
	UnsignedRightShiftEqual, // >>>=
	BitwiseAndEqual, // &=
	BitwiseXorEqual, // ^=
	BitwiseOrEqual, // |=
	Backslash, // '\'

	Identifier,
	String,
	Number,

	// Keywords
    Await,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Extends,
    False,
    Finally,
    For,
    Function,
    If,
    Implements,
    Import,
    In,
    Instanceof,
    Interface,
    Let,
    New,
    Null,
    Package,
    Private,
    Protected,
    Public,
    Return,
    Super,
    Switch,
    Static,
    This,
    Throw,
    Try,
    True,
    Typeof,
    Var,
    Void,
    While,
    With,
    Yield,

	Whitespace,
	Tab,
	NewLine,
	Comment,

	Eof,
	Invalid
};

struct Token
{
	TokenType Type;
	unsigned Line;
	unsigned Column;
	IPLString Lexeme;
	double Number;
};

struct LexerResult
{
	bool IsSuccessful;
	IPLError Error;
	IPLVector<Token> tokens;
};

struct LexerSettings
{
	bool CreateWhitespaceTokens;
	bool CreateCommentTokens;
};

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

class Tokenizer
{
public:
	Tokenizer(const char* code, const LexerSettings& settings);
	LexerResult Tokenize();

private:
	Token NextToken();

	Token ProduceSkipToken()
	{
		return ProduceToken(TokenType::Invalid, "");
	}

	Token ProduceToken(TokenType type)
	{
		switch (type)
		{
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
		case TokenType::Plus:
			return ProduceSingleCharacterToken(type, "+");
		case TokenType::Semicolon:
			return ProduceSingleCharacterToken(type, ";");
		case TokenType::Star:
			return ProduceSingleCharacterToken(type, "*");
		case TokenType::Division:
			return ProduceSingleCharacterToken(type, "/");
		case TokenType::Modulo:
			return ProduceSingleCharacterToken(type, "%");
		case TokenType::BitwiseNot:
			return ProduceSingleCharacterToken(type, "~");
		case TokenType::BitwiseAnd:
			return ProduceSingleCharacterToken(type, "&");
		case TokenType::BitwiseXor:
			return ProduceSingleCharacterToken(type, "^");
		case TokenType::BitwiseOr:
			return ProduceSingleCharacterToken(type, "|");
		case TokenType::QuestionMark:
			return ProduceSingleCharacterToken(type, "?");
		case TokenType::Colon:
			return ProduceSingleCharacterToken(type, ":");
		case TokenType::LeftSquareBracket:
			return ProduceSingleCharacterToken(type, "[");
		case TokenType::RightSquareBracket:
			return ProduceSingleCharacterToken(type, "]");
		case TokenType::LogicalNot:
			return ProduceSingleCharacterToken(type, "!");
		case TokenType::Equal:
			return ProduceSingleCharacterToken(type, "=");
		case TokenType::Arrow:
			return ProduceSingleCharacterToken(type, "=>");
		case TokenType::NotEqual:
			return ProduceSingleCharacterToken(type, "!=");
		case TokenType::EqualEqual:
			return ProduceSingleCharacterToken(type, "==");
		case TokenType::StrictEqual:
			return ProduceSingleCharacterToken(type, "===");
		case TokenType::StrictNotEqual:
			return ProduceSingleCharacterToken(type, "!==");
		case TokenType::Greater:
			return ProduceSingleCharacterToken(type, ">");
		case TokenType::GreaterEqual:
			return ProduceSingleCharacterToken(type, ">=");
		case TokenType::Less:
			return ProduceSingleCharacterToken(type, "<");
		case TokenType::LessEqual:
			return ProduceSingleCharacterToken(type, "<=");
		case TokenType::StarStar:
			return ProduceSingleCharacterToken(type, "**");
		case TokenType::MinusMinus:
			return ProduceSingleCharacterToken(type, "--");
		case TokenType::PlusPlus:
			return ProduceSingleCharacterToken(type, "++");
		case TokenType::LeftShift:
			return ProduceSingleCharacterToken(type, "<<");
		case TokenType::RightShift:
			return ProduceSingleCharacterToken(type, ">>");
		case TokenType::UnsignedRightShift:
			return ProduceSingleCharacterToken(type, ">>>");
		case TokenType::LogicalAnd:
			return ProduceSingleCharacterToken(type, "&&");
		case TokenType::LogicalOr:
			return ProduceSingleCharacterToken(type, "||");
		case TokenType::StarEqual:
			return ProduceSingleCharacterToken(type, "*=");
		case TokenType::StarStarEqual:
			return ProduceSingleCharacterToken(type, "**=");
		case TokenType::DivisionEqual:
			return ProduceSingleCharacterToken(type, "/=");
		case TokenType::ModuloEqual:
			return ProduceSingleCharacterToken(type, "%=");
		case TokenType::PlusEqual:
			return ProduceSingleCharacterToken(type, "+=");
		case TokenType::MinusEqual:
			return ProduceSingleCharacterToken(type, "-=");
		case TokenType::LeftShiftEqual:
			return ProduceSingleCharacterToken(type, "<<=");
		case TokenType::RightShiftEqual:
			return ProduceSingleCharacterToken(type, ">>=");
		case TokenType::UnsignedRightShiftEqual:
			return ProduceSingleCharacterToken(type, ">>>=");
		case TokenType::BitwiseAndEqual:
			return ProduceSingleCharacterToken(type, "&=");
		case TokenType::BitwiseXorEqual:
			return ProduceSingleCharacterToken(type, "^=");
		case TokenType::BitwiseOrEqual:
			return ProduceSingleCharacterToken(type, "|=");
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
	Token ProduceCommentToken(TokenType type, const IPLString& comment)
	{
		unsigned int lastTokenPosition = m_LastTokenPozition;
		m_LastTokenPozition = m_Column;
		return Token { type, m_Line, lastTokenPosition, "<p style=\"display:inline; color:green\">" + comment + "</p>", 0.0};
	}
	Token ProduceSingleCharacterToken(TokenType type, const IPLString& operatorLexeme)
	{
		unsigned int lastTokenPosition = m_LastTokenPozition;
		m_LastTokenPozition = m_Column;
		return Token{type, m_Line, lastTokenPosition, "<p style=\"display:inline; color:yellow\">" + operatorLexeme + "</p>", 0.0};
	}
	Token ProduceStringToken(TokenType type, const IPLString& str)
	{
		unsigned int lastTokenPosition = m_LastTokenPozition;
		m_LastTokenPozition = m_Column;
		return Token{type, m_Line, lastTokenPosition, "<p style=\"display:inline; color:purple\">" + str + "</p>", 0.0};
	}
	Token ProduceIdentifierToken(TokenType type, const IPLString& identifier)
	{
		unsigned int lastTokenPosition = m_LastTokenPozition;
		m_LastTokenPozition = m_Column;
		return Token{type, m_Line, lastTokenPosition, "<p style=\"display:inline; color:#FFF59E\">" + identifier + "</p>", 0.0};
	}
	Token ProduceKeywordToken(TokenType type, const IPLString& keyword)
	{
		unsigned int lastTokenPosition = m_LastTokenPozition;
		m_LastTokenPozition = m_Column;
		return Token{type, m_Line, lastTokenPosition, "<p style=\"display:inline; color:blue\">" + keyword + "</p>", 0.0};
	}
	Token ProduceToken(TokenType type, const IPLString& lexeme, double number = 0.0)
	{
		unsigned int lastTokenPosition = m_LastTokenPozition;
		m_LastTokenPozition = m_Column;
		return Token{type, m_Line, lastTokenPosition, lexeme, number};
	}
	Token ProduceErrorToken()
	{
		return ProduceToken(TokenType::Invalid, "");
	}
	Token ProduceInvalidToken()
	{
		SetError("Invalid or unexpected token");
		
		return ProduceToken(TokenType::Invalid, "");
	}

	bool FilterToken(TokenType type);

	IPLString ParseComment();
	double ParseNumber();
	IPLString ParseString();
	Keyword ParseKeyword();
	Identifier ParseIdentifier();

	bool IsStateSuccess() const
	{
		return m_GenerationState == State::Success;
	}
	bool IsStateError() const
	{
		return m_GenerationState == State::Error;
	}

	void SetError(const IPLString& what)
	{
		m_Error = IPLError{m_Line, m_Column, "", "Syntax error: " + what};
		m_GenerationState = State::Error;
	}

	bool Match(const char c);

	void NextSymbol() { ++m_Current; ++m_Column; }
	void PreviousSymbol() { --m_Current; --m_Column; }
	void NextLine() { ++m_Current; ++m_Line; m_LastTokenPozition = m_Column = 0; }

	unsigned m_Line;
	unsigned m_Column;
	unsigned m_Current;
	unsigned m_LastTokenPozition;
	const char* m_Code;

	IPLError m_Error;
	State m_GenerationState;

	LexerSettings m_Settings;

	IPLUnorderedMap<IPLString, TokenType> m_KeyWordsTable;

	// Helpers
	bool IsUpperCase(char c)
	{
		return c >= 'A' && c <= 'Z';
	}

	bool IsLowerCase(char c)
	{
		return c >= 'a' && c <= 'z';
	}

	bool IsDigit(char c)
	{
		return c >= '0' && c <= '9';
	}

	bool IsValidIdentifierStartingChar(char c)
	{
		return IsUpperCase(c) || IsLowerCase(c) || c == '_' || c == '$';
	}

	bool IsValidIdentifierChar(char c)
	{
		return IsValidIdentifierStartingChar(c) || IsDigit(c);
	}

	bool IsStringBound(char c)
	{
		return c == '"' || c == '\'';
	}

	bool IsNewLine(char c)
	{
		return c == '\n';
	}

	bool IsEnd(char c)
	{
		return c == '\0';
	}
};


LexerResult Tokenize(const char* code, const LexerSettings& settings);
LexerResult Tokenize(const char * code);