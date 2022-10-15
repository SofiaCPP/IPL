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
	Backslash,

	Identifier,
	String,
	Number,

	// Keywords. ECMAScript 2015
	Abstract,
	Package,
	Synchronize,
	Boolean,
	Private,
	Double,
	Implements,
	Protected,
	Byte,
	Public,
	Throws,
	Transient,
	Int,
	Short,
	Char,
	Final,
	Interface,
	Static,
	Long,
	Float,
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
	Let,
	Void,
	While,
	With,
	Yield,
	Null,
	Undefined,
	True,
	False,

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

LexerResult Tokenize(const char* code, const LexerSettings& settings);
LexerResult Tokenize(const char* code);