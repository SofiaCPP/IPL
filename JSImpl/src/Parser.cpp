#include "Parser.h"
#include <assert.h>

class Parser
{
public:
	Parser(const IPLVector<Token>& tokens, const std::function<void()>& onError = {});
	ExpressionPtr Parse();
private:
	bool MatchOneOf(IPLVector<TokenType> types);
	bool Match(TokenType type);
	ExpressionPtr RegularExpression();
	ExpressionPtr ParenthesizedExpression();
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
	ExpressionPtr OptionalExpression();

	//Statements
	ExpressionPtr Statement();
	ExpressionPtr EmptyStatement();
	ExpressionPtr VariableDefinition();
	ExpressionPtr Block();
	ExpressionPtr LabeledStatement();
	ExpressionPtr IfStatementfull();
	ExpressionPtr SwitchStatement();
	ExpressionPtr DoStatement();
	ExpressionPtr WhileStatement();
	ExpressionPtr ForStatement();
	ExpressionPtr WithStatement();
	ExpressionPtr ContinueStatement();
	ExpressionPtr BreakStatement();
	ExpressionPtr OptionalLabel();
	ExpressionPtr ReturnStatement();
	ExpressionPtr TryStatement();

	ExpressionPtr FunctionDefinition();
	ExpressionPtr Program();
	ExpressionPtr TopStatements();
	ExpressionPtr TopStatement();

	Token& Prev() { return m_Tokens[m_Current - 1]; }
	IPLVector<Token> m_Tokens;
	unsigned m_Current;
	std::function<void()> OnError;
};

Parser::Parser(const IPLVector<Token>& tokens, const std::function<void()>& onError)
	: m_Tokens(tokens)
	, m_Current(0)
	, OnError(onError)
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

ExpressionPtr Parser::RegularExpression()
{
	return nullptr;
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

ExpressionPtr  Parser::ParenthesizedExpression()
{
	if (Match(TokenType::LeftParen))
	{
		auto ex = Parser::Expression();
		if (Match(TokenType::RightParen))
		{
			return ex;
		}
		// TODO log error
		assert(false);
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
	//if (Match(TokenType::LeftBrace))
	//{
	//	auto LiteralField = [=]() -> ExpressionPtr {
	//		if (Match(TokenType::Identifier))
	//		{
	//			auto id = Prev().Lexeme;
	//			auto ae = AssignmentExpression();
	//			return IPLMakeSharePtr<IdentifierExpression>(id, ae);
	//		}
	//		return nullptr;
	//	};

	//	auto FieldList = [=]() -> ExpressionPtr {
	//		auto result = IPLMakeSharePtr<ListExpression>();
	//		while (auto lf = LiteralField())
	//		{
	//			result->Push(lf);
	//			if (!Match(TokenType::Comma))
	//			{
	//				break;
	//			}
	//		}
	//		return result;
	//	};
	//	auto fl = FieldList();
	//	if (Match(TokenType::RightBrace))
	//	{
	//		return fl;
	//	}
	//	// TODO Add error loging
	//	assert(false);
	//}
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
				result->GetValuesByRef().push_back(lf);
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
		return IPLMakeSharePtr<IdentifierExpression>(Prev().Lexeme);
	}
	else if (auto al = ArrayLiteral())
	{
		return al;
	}
	else if (auto p = ParenthesizedExpression())
	{
		return p;
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

	while(MatchOneOf({ TokenType::Star, TokenType::Division, TokenType::Modulo }))
	{
		auto type = Prev().Type;
		auto right = Unary();
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
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
	while (MatchOneOf({ TokenType::Plus, TokenType::Minus}))
	{
		auto type = Prev().Type;
		auto right = MultiplicativeExpression();
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
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
	while (MatchOneOf({ TokenType::LeftShift, TokenType::RightShift }))
	{
		auto type = Prev().Type;
		auto right = AdditiveExpression();
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
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
	while (MatchOneOf({ TokenType::Less,
		TokenType::Greater,
		TokenType::LessEqual,
		TokenType::GreaterEqual,
		TokenType::Instanceof,
		TokenType::In
	}))
	{
		auto type = Prev().Type;
		auto right = ShiftExpression();
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
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
	while (MatchOneOf({ TokenType::Equal,
		TokenType::BangEqual,
		TokenType::StrictEqual,
		TokenType::StrictNotEqual,
	}))
	{
		auto type = Prev().Type;
		auto right = RelationalExpression();
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
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
		while (Match(TokenType::BitwiseAnd))
		{
			auto type = Prev().Type;
			auto right = EqualityExpression();
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	auto BitwiseXorExpression = [=]() -> ExpressionPtr {
		//BitwiseXorExpression ->
		//	BitwiseAndExpression
		//	| BitwiseXorExpression ^ BitwiseAndExpression normal
		auto left = BitwiseAndExpression();
		while (Match(TokenType::BitwiseXor))
		{
			auto type = Prev().Type;
			auto right = BitwiseAndExpression();
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	std::function<ExpressionPtr()> BitwiseOrExpression;
	BitwiseOrExpression = [=]() -> ExpressionPtr {
		//BitwiseOrExpression ->
		//	BitwiseXorExpression
		//	| BitwiseOrExpression | BitwiseXorExpression normal
		auto left = BitwiseXorExpression();
		while (Match(TokenType::BitwiseOr))
		{
			auto type = Prev().Type;
			auto right = BitwiseXorExpression();
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	return BitwiseOrExpression();
}


ExpressionPtr Parser::LogicalExpression()
{
	std::function<ExpressionPtr()> LogicalAndExpression;
	LogicalAndExpression = [=]() -> ExpressionPtr {
		//LogicalAndExpression ->
		//	BitwiseOrExpression
		//	| LogicalAndExpression && BitwiseOrExpression normal
		auto left = BitwiseExpression();
		while (Match(TokenType::LogicalAnd))
		{
			auto type = Prev().Type;
			auto right = BitwiseExpression();
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	std::function<ExpressionPtr()> LogicalOrExpression;
	LogicalOrExpression = [=]() -> ExpressionPtr {
		//LogicalOrExpression
		//	LogicalAndExpression
		//	| LogicalOrExpression || LogicalAndExpression normal
		auto left = LogicalAndExpression();
		while (Match(TokenType::LogicalAnd))
		{
			auto type = Prev().Type;
			auto right = LogicalAndExpression();
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
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
	return condition;
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
	//Expression ->
	//	AssignmentExpression
	//	| Expression , AssignmentExpression normal
	auto ae = AssignmentExpression();
	while (Match(TokenType::Comma))
	{
		auto next = AssignmentExpression();
		if (next)
		{
			ae = IPLMakeSharePtr<BinaryExpression>(ae, next, Prev().Type);
		}
		else
		{
			// TODO log
			assert(false);
		}
	}
	return ae;
}


ExpressionPtr Parser::Statement()
{
	if (auto result = EmptyStatement()) return result;
	else if(auto result = VariableDefinition()) return result;
	else if(auto result = Block()) return result;
	else if(auto result = LabeledStatement()) return result;
	else if(auto result = IfStatementfull()) return result;
	else if(auto result = SwitchStatement()) return result;
	else if(auto result = DoStatement()) return result;
	else if(auto result = WhileStatement()) return result;
	else if(auto result = ForStatement()) return result;
	else if(auto result = WithStatement()) return result;
	else if(auto result = ContinueStatement()) return result;
	else if(auto result = BreakStatement()) return result;
	else if(auto result = OptionalLabel()) return result;
	else if(auto result = ReturnStatement()) return result;
	else if(auto result = TryStatement()) return result;
	return nullptr;
}

ExpressionPtr Parser::EmptyStatement()
{
	if (Match(TokenType::Semicolon))
	{
		// Not sure what to do here;
		// This just will consume ;
		return nullptr;
	}
	return nullptr;
}

ExpressionPtr Parser::VariableDefinition()
{
	auto VariableDeclaration = [=]() -> ExpressionPtr {
		//	VariableDeclaration -> Identifier VariableInitializer
		//	VariableInitializer ->
		//	«empty»
		//	| = AssignmentExpressionnormal,
		if (Match(TokenType::Identifier))
		{
			auto id = Prev().Lexeme;
			auto ae = CreateEmptyExpression();
			if (Match(TokenType::Equal))
			{
				auto ae = AssignmentExpression();
			}
			return IPLMakeSharePtr<VariableDefinitionExpression>(id, ae);
		}
		return nullptr;
	};

	auto VariableDeclarationList = [=]() -> ExpressionPtr {
		//	VariableDeclarationList ->
		//	VariableDeclaration
		//	| VariableDeclarationList, VariableDeclaration
		auto vdList = IPLMakeSharePtr<ListExpression>();
		vdList->GetValuesByRef().push_back(VariableDeclaration());
		while (Match(TokenType::Comma))
		{
			vdList->GetValuesByRef().push_back(VariableDeclaration());
		}
		return vdList;
	};

	//VariableDefinition -> var VariableDeclarationListallowIn
	if (Match(TokenType::Var))
	{
		return VariableDeclarationList();
	}

	return nullptr;
}

ExpressionPtr Parser::Block()
{
	auto BlockStatementsPrefix = [=]() -> ExpressionPtr {
		//	BlockStatementsPrefix ->
		//	Statement full
		//	| BlockStatementsPrefix Statement full

		//return Statement;
		auto StatementsList = IPLMakeSharePtr<BlockStatement>();
		while (auto s = Statement())
		{
			StatementsList->GetValuesByRef().push_back(s);
		}
		return StatementsList;
	};

	if (Match(TokenType::LeftBrace))
	{
		if (Match(TokenType::RightBrace))
		{
			return BlockStatementsPrefix();
		}
		else
		{
			// TODO log error
			assert(false);
			return nullptr;
		}
	}
	return nullptr;
}

ExpressionPtr Parser::LabeledStatement()
{
	if (Match(TokenType::Identifier))
	{
		auto identifier = Prev().Lexeme;
		auto stament = Statement();
		return IPLMakeSharePtr<::LabeledStatement>(identifier, stament);
	}
	return nullptr;
}

ExpressionPtr Parser::IfStatementfull()
{
	if (Match(TokenType::If))
	{
		auto cond = ParenthesizedExpression();
		auto ifBody = Statement();
		ExpressionPtr elseBody = nullptr;
		if (Match(TokenType::Else))
		{
			elseBody = Statement();
		}
		return IPLMakeSharePtr<::IfStatement>(cond, ifBody, elseBody);
	}
	return nullptr;
}

ExpressionPtr Parser::SwitchStatement()
{
	if (Match(TokenType::Switch))
	{
		auto cond = ParenthesizedExpression();
		IPLVector<ExpressionPtr> cases;
		ExpressionPtr defaultCase = nullptr;
		if (Match(TokenType::LeftBrace))
		{

			while (!Match(TokenType::RightBrace))
			{
				if (Match(TokenType::Case))
				{
					auto expr = Expression();
					if (Match(TokenType::Colon))
					{
						auto statement = Statement();
						cases.push_back(IPLMakeSharePtr<CaseStatement>(expr, statement));
					}
				}
				else if (Match(TokenType::Default))
				{
					if (Match(TokenType::Colon))
					{
						defaultCase = Statement();
					}
				}
			}
			return IPLMakeSharePtr<::SwitchStatement>(cond, cases, defaultCase);
		}
	}
	return nullptr;
}

ExpressionPtr Parser::DoStatement()
{
	if(Match(TokenType::Do))
	{
		auto body = Statement();
		if (Match(TokenType::While))
		{
			auto cond = ParenthesizedExpression();
			auto isDoWhile = true;
			return IPLMakeSharePtr<::WhileStatement>(cond, body, isDoWhile);
		}
		// TODO log error
	}
	return nullptr;
}

ExpressionPtr Parser::WhileStatement()
{
	if (Match(TokenType::While))
	{
		auto cond = ParenthesizedExpression();
		auto body = Statement();
		auto isDoWhile = false;
		return IPLMakeSharePtr<::WhileStatement>(cond, body, isDoWhile);
	}
	return nullptr;
}

ExpressionPtr Parser::ForStatement()
{
	if (Match(TokenType::For))
	{
		auto initializer = Expression();
		if (!initializer)
		{
			initializer = VariableDefinition();
		}

		auto cond = Expression();
		auto iteration = Expression();
		auto body = Statement();
		return IPLMakeSharePtr<::ForStatement>(initializer, cond, iteration, body);
	}
	return nullptr;
}

ExpressionPtr Parser::WithStatement()
{
	return nullptr;
}

ExpressionPtr Parser::ContinueStatement()
{
	if (Match(TokenType::Continue))
	{
		auto type = TokenType::Continue;
		ExpressionPtr expr = OptionalLabel();
		bool suffix = true;
		return IPLMakeSharePtr<UnaryExpression>(expr, type, suffix);
	}
	return nullptr;
}

ExpressionPtr Parser::BreakStatement()
{
	if (Match(TokenType::Break))
	{
		auto type = TokenType::Break;
		ExpressionPtr expr = OptionalLabel();
		bool suffix = true;
		return IPLMakeSharePtr<UnaryExpression>(expr, type, suffix);
	}
	return nullptr;
}

ExpressionPtr Parser::OptionalLabel()
{
	if (Match(TokenType::Identifier))
	{
		return  IPLMakeSharePtr<IdentifierExpression>(Prev().Lexeme);
	}
	return nullptr;
}

ExpressionPtr Parser::ReturnStatement()
{
	if (Match(TokenType::Return))
	{
		auto type = TokenType::Return;
		ExpressionPtr expr = OptionalExpression();
		bool suffix = true;
		return IPLMakeSharePtr<UnaryExpression>(expr, type, suffix);
	}
	return nullptr;
}

ExpressionPtr Parser::TryStatement()
{
	return nullptr;
}

ExpressionPtr Parser::OptionalExpression()
{
	//OptionalExpression ->
	//	Expressionnormal, allowIn
	//	| «empty»
	return Parser::Expression();
}

ExpressionPtr Parser::Parse()
{
	return Parser::Program();
}

ExpressionPtr Parser::FunctionDefinition()
{
	auto  FormalParameters = [=](IPLVector<IPLString> identifiers) -> bool {
		if (Match(TokenType::LeftParen))
		{
			while (Match(TokenType::Identifier))
			{
				identifiers.push_back(Prev().Lexeme);
				if (!Match(TokenType::Comma))
				{
					break;
				}
			}

			if (Match(TokenType::RightParen))
			{
				return true;
			}
		}
		return false;
	};

	auto Body = [=]() -> ExpressionPtr {

		if (Match(TokenType::LeftBrace))
		{
			auto ts = TopStatements();
			if (Match(TokenType::RightBrace))
			{
				return ts;
			}
		}
		return nullptr;
	};

	if (Match(TokenType::Function))
	{
		if (Match(TokenType::Identifier))
		{
			auto name = Prev().Lexeme;
			IPLVector<IPLString> identifiers;
			if (FormalParameters(identifiers))
			{
				if (auto body = Body())
				{
					return IPLMakeSharePtr<FunctionDeclaration>(name, identifiers, body);
				}
				else
				{
					// TODO log error
					return nullptr;
				}
			}
		}
	}
	return nullptr;
}


ExpressionPtr Parser::Program()
{
	return TopStatements();
}

ExpressionPtr Parser::TopStatements()
{
	auto statements = IPLMakeSharePtr<::TopStatements>();
	while (auto ts = TopStatement())
	{
		statements->GetValuesByRef().push_back(ts);
	}
	return statements;
}

ExpressionPtr Parser::TopStatement()
{
	auto result = FunctionDefinition();
	if (!result)
	{
		result = Statement();
	}
	return result;
}

ExpressionPtr Parse(const IPLVector<Token>& tokens, const std::function<void()>& onError)
{
	Parser p(tokens, onError);
	return p.Parse();
}
