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
	Minums,
	Plus,
	Semicolon,
	Slash,
	Star,

	// One or two character tokens
	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,

	Identifier,
	String,
	Number,

	// Keywords.
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
