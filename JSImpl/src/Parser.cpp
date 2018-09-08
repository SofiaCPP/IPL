#include "Parser.h"
#include <assert.h>

class Parser
{
public:
	Parser(IPLVector<Token>& tokens, const std::function<void()>& onError = {});
	ExpressionPtr Parse();
private:
	bool MatchOneOf(IPLVector<TokenType> types);
	bool Match(TokenType type);
	ExpressionPtr PrimaryExpression();
	ExpressionPtr FunctionExpression();
	ExpressionPtr ObjectLiteral();
	ExpressionPtr ArrayLiteral();
	ExpressionPtr AnonymousFunction();
	ExpressionPtr NamedFunction();
	ExpressionPtr Unary();
	ExpressionPtr LeftSideExpression();
	ExpressionPtr SimpleExpression();
	ExpressionPtr MultiplicativeExpression();
	ExpressionPtr AdditiveExpression();
	ExpressionPtr ShiftExpression();
	ExpressionPtr RelationalExpression();
	ExpressionPtr EqualityExpression();
	ExpressionPtr BitwiseExpression();
	ExpressionPtr LogicalExpression();
	ExpressionPtr ConditionalExpression();
	ExpressionPtr AssignmentExpression();
	ExpressionPtr Expression();
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

ExpressionPtr Parser::PrimaryExpression()
{
	if (auto simple = SimpleExpression())
	{
		return simple;
	}
	else if(auto  function = FunctionExpression())
	{
		return function;
	}
	else if (auto object = ObjectLiteral())
	{
		return object;
	}
	return nullptr;
}

ExpressionPtr Parser::FunctionExpression()
{
	if (auto aFunc = AnonymousFunction())
	{
		return aFunc;
	}
	else if (auto nFunc = NamedFunction())
	{
		return nFunc;
	}
	return nullptr;
}

ExpressionPtr Parser::ObjectLiteral()
{
	//ObjectLiteral ->
	//{}
	//	| { FieldList }
	//FieldList ->
	//	LiteralField
	//	| FieldList, LiteralField
	//	LiteralField -> Identifier : AssignmentExpressionnormal, allowIn
	if (Match(TokenType::LeftBrace))
	{
		auto LiteralField = [=]() -> ExpressionPtr {
			if (Match(TokenType::Identifier))
			{
				auto ae = AssignmentExpression();
				return IPLMakeSharePtr<IdentifierExpression>(Prev().Lexeme, ae);
			}
			return nullptr;
		};

		auto FieldList = [=]() -> ExpressionPtr {
			auto result = IPLMakeSharePtr<ListExpression>();
			while (auto lf = LiteralField())
			{
				result->Push(lf);
				if (!Match(TokenType::Comma))
				{
					break;
				}
			}
			return result;
		};
		auto fl = FieldList();
		if (Match(TokenType::RightBrace))
		{
			return fl;
		}
		// TODO Add error loging
		assert(false);
	}
	return nullptr;
}

ExpressionPtr Parser::ArrayLiteral()
{
	//ArrayLiteral ->
	//	[]
	//| [ElementList]
	if (Match(TokenType::LeftSquareBracket))
	{
		//	ElementList ->
		//	LiteralElement -> AssignmentExpressionnormal, allowIn
		auto LiteralElement = [=]() -> ExpressionPtr {
			return AssignmentExpression();
		};

		auto ElementList = [=]() -> ExpressionPtr {
			//	LiteralElement
			//	| ElementList, LiteralElement
			auto result = IPLMakeSharePtr<ListExpression>();
			while (auto lf = LiteralElement())
			{
				result->Push(lf);
				if (!Match(TokenType::Comma))
				{
					break;
				}
			}
			return result;
		};

		auto el = ElementList();
		if (Match(TokenType::RightSquareBracket)) {
			return el;
		}
		// TODO Add error loging
		assert(false);
	}
	return nullptr;
}

ExpressionPtr Parser::AnonymousFunction()
{
	return nullptr;
}

ExpressionPtr Parser::NamedFunction()
{
	return nullptr;
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
	if (Match(TokenType::Number))
	{
		return IPLMakeSharePtr<LiteralExpression>(Prev().Number);
	}
	else if (Match(TokenType::String))
	{
		return IPLMakeSharePtr<LiteralExpression>(Prev().Lexeme);
	}
	else if (Match(TokenType::Null))
	{
		return IPLMakeSharePtr<LiteralExpression>(Prev().Type);
	}
	else if (Match(TokenType::Undefined))
	{
		return IPLMakeSharePtr<LiteralExpression>(Prev().Type);
	}
	else if (Match(TokenType::True))
	{
		bool t = true;
		return IPLMakeSharePtr<LiteralExpression>(t);
	}
	else if (Match(TokenType::False))
	{
		bool f = false;
		return IPLMakeSharePtr<LiteralExpression>(f);
	}
	else if (Match(TokenType::Identifier))
	{
		ExpressionPtr empty;
		return IPLMakeSharePtr<IdentifierExpression>(Prev().Lexeme, empty);
	}
	else if (auto al = ArrayLiteral())
	{
		return al;
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

ExpressionPtr Parser::BitwiseExpression()
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
		if (Match(TokenType::BitwiseXor))
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
		if (Match(TokenType::BitwiseOr))
		{
			auto right = BitwiseXorExpression();
			return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
		}
		return left;
	};

	return BitwiseOrExpression();
}


ExpressionPtr Parser::LogicalExpression()
{
	auto LogicalAndExpression = [=]() -> ExpressionPtr {
		//LogicalAndExpression ->
		//	BitwiseOrExpression
		//	| LogicalAndExpression && BitwiseOrExpression normal
		auto left = BitwiseExpression();
		if (Match(TokenType::LogicalAnd))
		{
			auto right = BitwiseExpression();
			return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
		}
		return left;
	};

	auto LogicalOrExpression = [=]() -> ExpressionPtr {
		//LogicalOrExpression
		//	LogicalAndExpression
		//	| LogicalOrExpression || LogicalAndExpression normal
		auto left = LogicalAndExpression();
		if (Match(TokenType::LogicalAnd))
		{
			auto right = LogicalAndExpression();
			return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
		}
		return left;
	};

	return LogicalOrExpression();
}

ExpressionPtr Parser::ConditionalExpression()
{
	//ConditionalExpression ->
	//	LogicalOrExpression
	//	| LogicalOrExpression ? AssignmentExpression normal : AssignmentExpression normal
	auto condition = LogicalExpression();
	if (Match(TokenType::QuestionMark))
	{
		auto trueExpr= AssignmentExpression();
		if (Match(TokenType::Colon))
		{
			auto falseExpr = AssignmentExpression();
			// TODO implement trinary operator
			assert(false);
			return ExpressionPtr();
		}
		// TODO log parsing error
		assert(false);
	}
	return LogicalExpression();
}

ExpressionPtr Parser::AssignmentExpression()
{
	//AssignmentExpression ->
	//	ConditionalExpression
	//	| LeftSideExpression = AssignmentExpression normal
	//	| LeftSideExpression CompoundAssignment AssignmentExpression normal

	auto snapShot = m_Current;
	auto left = LeftSideExpression();
	if (MatchOneOf({ TokenType::Equal,
		TokenType::StarEqual,
		TokenType::DivideEqual,
		TokenType::ModuloEqual,
		TokenType::PlusEqual,
		TokenType::MinusEqual,
		TokenType::LeftShiftEqual,
		TokenType::RightShiftEqual,
		TokenType::BitwiseAndEqual,
		TokenType::BitwiseXorEqual,
		TokenType::BitwiseOrEqual }))
	{
		auto right = AssignmentExpression();
		return IPLMakeSharePtr<BinaryExpression>(left, right, Prev().Type);
	}
	// revert state;
	m_Current = snapShot;
	return ConditionalExpression();
}

ExpressionPtr Parser::Expression()
{
	assert(false);
	return nullptr;
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