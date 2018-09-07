#include "Parser.h"

class Parser
{
public:
	Parser(IPLVector<Token>& tokens, const std::function<void()>& onError = {});
	ExpressionPtr Parse();
private:
	bool MatchOneOf(IPLVector<TokenType> types);
	bool Match(TokenType type);
	ExpressionPtr Unary();
	ExpressionPtr LeftSideExpression();
	ExpressionPtr SimpleExpression();

	Token& Prev() { return m_Tokens[m_Current - 1]; }
	std::function<void()> OnError;
	unsigned m_Current;
	IPLVector<Token> m_Tokens;
};

Parser::Parser(IPLVector<Token>& tokens, const std::function<void()>& onError)
	: m_Tokens(tokens)
	, OnError(onError)
	, m_Current(0)
{
}

bool Parser::MatchOneOf(IPLVector<TokenType> types)
{
	for (auto t : types)
	{
		if (t == m_Tokens[m_Current].Type)
		{
			++m_Current;
			return true;
		}
	}
	return false;
}

bool Parser::Match(TokenType type)
{
	if (type == m_Tokens[m_Current].Type)
	{
		++m_Current;
		return true;
	}
	return false;
}

ExpressionPtr Parser::SimpleExpression()
{
	//SimpleExpression
	//	this
	//	| null
	//	| true
	//	| false
	//	| Number
	//	| String
	//	| Identifier
	//	| RegularExpression
	//	| ParenthesizedExpression
	//	| ArrayLiteral
	if (Match({TokenType::Number}))
	{
		return IPLMakeSharePtr<LiteralExpression>(Prev().Number);
	}
	else if (Match({ TokenType::String }))
	{
		return IPLMakeSharePtr<LiteralExpression>(Prev().Lexeme);
	}
	else
	{
		// TODO implement
		return ExpressionPtr();
	}
}

ExpressionPtr Parser::LeftSideExpression()
{
	//LeftSideExpression ->
	//	CallExpression
	//	| ShortNewExpression

	//	CallExpression ->
	//	PrimaryExpression
	//	| FullNewExpression
	//	| CallExpression MemberOperator
	//	| CallExpression Arguments

	//	FullNewExpression -> new FullNewSubexpression Arguments

	//	ShortNewExpression -> new ShortNewSubexpression

	//	FullNewSubexpression ->
	//	PrimaryExpressionnormal
	//	| FullNewExpression
	//	| FullNewSubexpression MemberOperator

	//	ShortNewSubexpression ->
	//	FullNewSubexpression
	//	| ShortNewExpression

	//	MemberOperator ->
	//	[Expressionnormal, allowIn]
	//| .Identifier

	//	Arguments ->
	//	()
	//	| (ArgumentList)

	//	ArgumentList ->
	//	AssignmentExpressionnormal, allowIn
	//	| ArgumentList, AssignmentExpressionnormal, allowIn
	return SimpleExpression();
}

ExpressionPtr Parser::Unary()
{

	//UnaryExpression
	//	PostfixExpression
	//	| delete LeftSideExpressionnormal
	//	| ++LeftSideExpressionnormal
	//	| --LeftSideExpressionnormal
	//	| void UnaryExpressionnormal
	//	| typeof UnaryExpressionnormal
	//	| +UnaryExpressionnormal
	//	| -UnaryExpressionnormal
	//	| ~UnaryExpressionnormal
	//	| !UnaryExpressionnormal
	if (MatchOneOf({ 
		TokenType::Delete,
		TokenType::MinusMinus,
		TokenType::PlusPlus,
		}))
	{
		auto ls = LeftSideExpression();
		auto suffix = false;
		return IPLMakeSharePtr<UnaryExpression>(ls, Prev().Type, suffix);
	}
	else if (MatchOneOf({
		TokenType::Void,
		TokenType::Typeof,
		TokenType::Plus,
		TokenType::Minus,
		TokenType::BitwiseNot,
		TokenType::Bang,
	}))
	{
		auto ls = Unary();
		auto suffix = false;
		return IPLMakeSharePtr<UnaryExpression>(ls, Prev().Type, suffix);
	}
	else
	{
		//PostfixExpression
		//	LeftSideExpression
		//	| LeftSideExpression++
		//	| LeftSideExpression--
		auto leftSide = LeftSideExpression();
		if (MatchOneOf({ TokenType::PlusPlus, TokenType::MinusMinus }))
		{
			auto suffix = true;
			return IPLMakeSharePtr<UnaryExpression>(leftSide, Prev().Type, suffix);
		}
		return leftSide;
	}
}

ExpressionPtr Parser::Parse()
{
	// TODO implement full grammar
	return Unary();
}


ExpressionPtr Parse(IPLVector<Token>& tokens, const std::function<void()>& onError)
{
	Parser p(tokens, onError);
	return p.Parse();
}