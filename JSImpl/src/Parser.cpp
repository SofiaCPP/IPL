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
	ExpressionPtr MultiplicativeExpression();
	ExpressionPtr AdditiveExpression();
	ExpressionPtr ShiftExpression();
	ExpressionPtr RelationalExpression();
	ExpressionPtr EqualityExpression();
	ExpressionPtr BitwiseAndExpression();
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

ExpressionPtr Parser::MultiplicativeExpression()
{
	//MultiplicativeExpression
	//	UnaryExpression
	//	| MultiplicativeExpression * UnaryExpression normal
	//	| MultiplicativeExpression / UnaryExpression normal
	//	| MultiplicativeExpression % UnaryExpression normal
	auto left = Unary();
	if (MatchOneOf({ TokenType::Star, TokenType::Division, TokenType::Modulo }))
	{
		auto right = Unary();
		return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
	}
	return left;
}

ExpressionPtr Parser::AdditiveExpression()
{
	//AdditiveExpression
	//	MultiplicativeExpression
	//	| AdditiveExpression + MultiplicativeExpression normal
	//	| AdditiveExpression - MultiplicativeExpression normal
	auto left = MultiplicativeExpression();
	if (MatchOneOf({ TokenType::Plus, TokenType::Minus}))
	{
		auto right = MultiplicativeExpression();
		return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
	}
	return left;
}

ExpressionPtr Parser::ShiftExpression()
{
	//ShiftExpression
	//	AdditiveExpression
	//	| ShiftExpression << AdditiveExpression normal
	//	| ShiftExpression >> AdditiveExpression normal
	//	| ShiftExpression >> > AdditiveExpression normal
	auto left = AdditiveExpression();
	if (MatchOneOf({ TokenType::LeftShift, TokenType::RightShift }))
	{
		auto right = AdditiveExpression();
		return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
	}
	return left;
}

ExpressionPtr Parser::RelationalExpression()
{
	//RelationalExpression, allowIn ->
	//	ShiftExpression
	//	| RelationalExpression, allowIn < ShiftExpression normal
	//	| RelationalExpression, allowIn > ShiftExpression normal
	//	| RelationalExpression, allowIn <= ShiftExpression normal
	//	| RelationalExpression, allowIn >= ShiftExpression normal
	//	| RelationalExpression, allowIn instanceof ShiftExpression normal
	//	| RelationalExpression, allowIn in ShiftExpression normal
	//	RelationalExpression, noIn ->
	//	ShiftExpression
	//	| RelationalExpression, noIn < ShiftExpression normal
	//	| RelationalExpression, noIn > ShiftExpression normal
	//	| RelationalExpression, noIn <= ShiftExpression normal
	//	| RelationalExpression, noIn >= ShiftExpression normal
	//	| RelationalExpression, noIn instanceof ShiftExpression normal
	auto left = ShiftExpression();
	if (MatchOneOf({ TokenType::Less,
		TokenType::Greater,
		TokenType::LessEqual,
		TokenType::GreaterEqual,
		TokenType::Instanceof,
		TokenType::In
	}))
	{
		auto right = ShiftExpression();
		return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
	}
	return left;
}

ExpressionPtr Parser::EqualityExpression()
{
	//EqualityExpression
	//	RelationalExpression
	//	| EqualityExpression == RelationalExpression normal
	//	| EqualityExpression != RelationalExpression normal
	//	| EqualityExpression == = RelationalExpression normal
	//	| EqualityExpression != = RelationalExpression normal
	auto left = RelationalExpression();
	if (MatchOneOf({ TokenType::Equal,
		TokenType::BangEqual,
		TokenType::StrictEqual,
		TokenType::StrictNotEqual,
	}))
	{
		auto right = RelationalExpression();
		return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
	}
	return left;
}

ExpressionPtr Parser::BitwiseAndExpression()
{
	auto BitwiseAndExpression = [=]() -> ExpressionPtr {
		//BitwiseAndExpression ->
		//	EqualityExpression
		//	| BitwiseAndExpression & EqualityExpression normal
		auto left = EqualityExpression();
		if (Match(TokenType::BitwiseAnd))
		{
			auto right = RelationalExpression();
			return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
		}
		return left;
	};

	auto BitwiseXorExpression = [=]() -> ExpressionPtr {
		//BitwiseXorExpression ->
		//	BitwiseAndExpression
		//	| BitwiseXorExpression ^ BitwiseAndExpression normal
		auto left = BitwiseAndExpression();
		if (Match(TokenType::BitwiseAnd))
		{
			auto right = BitwiseAndExpression();
			return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
		}
		return left;
	};

	auto BitwiseOrExpression = [=]() -> ExpressionPtr {
		//BitwiseOrExpression ->
		//	BitwiseXorExpression
		//	| BitwiseOrExpression | BitwiseXorExpression normal
		auto left = BitwiseXorExpression();
		if (Match(TokenType::BitwiseAnd))
		{
			auto right = BitwiseXorExpression();
			return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
		}
		return left;
	};

	return BitwiseOrExpression();
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