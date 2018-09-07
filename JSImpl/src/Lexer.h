#pragma once

#include "CommonTypes.h"
#include <functional>

enum TokenType
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

	Eof,
};

struct Token
{
	TokenType Type;
	unsigned Line;
	IPLString Lexeme;
	double Number;
};

IPLVector<Token> Tokenize(const char* code, const std::function<void()>& onError = {});
