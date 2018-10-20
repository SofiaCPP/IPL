#pragma once

#include "CommonTypes.h"

enum class TokenType
{
	// Single-character tokens.
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	Comma,
	Dot,
	Minus,
	Plus,
	Semicolon,
	Star,
	Division,
	Modulo,
	BitwiseNot,
	BitwiseAnd,
	BitwiseXor,
	BitwiseOr,
	QuestionMark,
	Colon,
	LeftSquareBracket,
	RightSquareBracket,

	// One or two character tokens
	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	StrictEqual,
	StrictNotEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
	MinusMinus,
	PlusPlus,
	LeftShift,
	RightShift,
	LogicalAnd,
	LogicalOr,
	StarEqual,
	DivideEqual,
	ModuloEqual,
	PlusEqual,
	MinusEqual,
	LeftShiftEqual,
	RightShiftEqual,
	BitwiseAndEqual,
	BitwiseXorEqual,
	BitwiseOrEqual,

	Identifier,
	String,
	Number,

	// Keywords. ECMAScript 2015
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
	Export,
	Extends,
	Finally,
	For,
	Function,
	If,
	Import,
	In,
	Instanceof,
	New,
	Return,
	Super,
	Switch,
	This,
	Throw,
	Try,
	Typeof,
	Var,
	Void,
	While,
	With,
	Yield,
	Null,
	Undefined,
	True,
	False,

	Whitespace,

	Eof,
	Invalid
};

struct Token
{
	TokenType Type;
	unsigned Line;
	IPLString Lexeme;
	double Number;
};

struct LexerResult
{
	bool IsSuccessful;
	IPLError Error;
};

struct LexerSettings
{
	bool CreateWhitespaceTokens;
};

LexerResult Tokenize(const char* code, IPLVector<Token>& tokens, const LexerSettings& settings);
