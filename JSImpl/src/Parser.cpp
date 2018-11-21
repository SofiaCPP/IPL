#include "Parser.h"

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
	ExpressionPtr Unary();
	ExpressionPtr LeftSideExpression();
	ExpressionPtr CallExpression();
	ExpressionPtr CallExpressionHelper();
	ExpressionPtr ShortNewExpression();
	ExpressionPtr ShortNewSubexpression();
	ExpressionPtr FullNewExpression();
	ExpressionPtr FullNewSubexpression();
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
	ExpressionPtr Arguments();

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

	struct InternalState
	{
		unsigned Current;
	};

	struct Location
	{
		unsigned Line;
		unsigned Column;
	};
	Location GetLocation() const { return { m_Tokens[m_Current].Line , m_Tokens[m_Current].Column }; }

	InternalState Snapshot();
	void Restore(const InternalState& state);

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

ExpressionPtr Parser::ParenthesizedExpression()
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
	auto  FormalParameters = [&](IPLVector<IPLString>& identifiers) -> bool {
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

	auto Body = [&]() -> ExpressionPtr {

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
		IPLString name;
		if (Match(TokenType::Identifier))
		{
			name = Prev().Lexeme;
		}
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
		// TODO log error
		return nullptr;
	}
	return nullptr;
}

ExpressionPtr Parser::ObjectLiteral()
{
	//if (Match(TokenType::LeftBrace))
	//{
	//	auto LiteralField = [&]() -> ExpressionPtr {
	//		if (Match(TokenType::Identifier))
	//		{
	//			auto id = Prev().Lexeme;
	//			ExpressionPtr ae;
	//			if (Match(TokenType::Colon))
	//			{
	//				ae = AssignmentExpression();
	//			}
	//			return IPLMakeSharePtr<VariableDefinitionExpression>(id, ae);
	//		}
	//		return nullptr;
	//	};

	//	auto FieldList = [&]() -> ExpressionPtr {
	//		auto result = IPLMakeSharePtr<LiteralObject>();
	//		auto lf = LiteralField();
	//		while (lf)
	//		{
	//			result->GetValuesByRef().push_back(lf);
	//			if (!Match(TokenType::Comma))
	//			{
	//				break;
	//			}
	//			lf = LiteralField();
	//			if (lf)
	//			{
	//				// TODO log error
	//				return nullptr;
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
	if (Match(TokenType::LeftSquareBracket))
	{
		auto LiteralElement = [&]() -> ExpressionPtr {
			return AssignmentExpression();
		};

		auto ElementList = [&]() -> ExpressionPtr {
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

ExpressionPtr Parser::SimpleExpression()
{
	if (Match(TokenType::Number))
	{
		return IPLMakeSharePtr<LiteralNumber>(Prev().Number);
	}
	else if (Match(TokenType::String))
	{
		return IPLMakeSharePtr<LiteralString>(Prev().Lexeme);
	}
	else if (Match(TokenType::Null))
	{
		return IPLMakeSharePtr<LiteralNull>();
	}
	else if (Match(TokenType::Undefined))
	{
		return IPLMakeSharePtr<LiteralUndefined>();
	}
	else if (Match(TokenType::True))
	{
		bool t = true;
		return IPLMakeSharePtr<LiteralBoolean>(t);
	}
	else if (Match(TokenType::False))
	{
		bool f = false;
		return IPLMakeSharePtr<LiteralBoolean>(f);
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
	ExpressionPtr result;
	auto ss = Snapshot();
	if (result = CallExpression())
	{
		return result;
	}
	Restore(ss);
	if (result = ShortNewExpression())
	{
		return result;
	}
	// TODO error
	return nullptr;
}

ExpressionPtr Parser::CallExpression()
{
	ExpressionPtr result;
	auto ss = Snapshot();
	if (result = PrimaryExpression())
	{
		return result;
	}
	Restore(ss);
	if (result = FullNewExpression())
	{
		return result;
	}

	if (result = CallExpressionHelper())
	{
		return result;
	}
	// TODO error
	return nullptr;
}

ExpressionPtr Parser::CallExpressionHelper()
{
	if (Match(TokenType::LeftSquareBracket))
	{
		auto expr = Expression();
		if (Match(TokenType::RightSquareBracket))
		{
			// return  meaningfull expr
			auto next = CallExpressionHelper();
			return nullptr;
		}
		// TODO error
		return nullptr;
	}
	if (Match(TokenType::Dot))
	{
		if (Match(TokenType::Identifier))
		{
			auto next = CallExpressionHelper();
			// return  meaningfull expr
			return nullptr;
		}
		// TODO error
		return nullptr;
	}

	if (auto args = Arguments())
	{
		// return  meaningfull expr
		auto next = CallExpressionHelper();
		return nullptr;
	}

	return nullptr;
};

ExpressionPtr Parser::ShortNewExpression()
{
	if (Match(TokenType::New))
	{
		return ShortNewSubexpression();
	}
	return nullptr;
}

ExpressionPtr Parser::ShortNewSubexpression()
{
	ExpressionPtr result;
	auto ss = Snapshot();
	if (result = FullNewSubexpression())
	{
		return result;
	}
	Restore(ss);
	if (result = ShortNewExpression())
	{
		return result;
	}
	// TODO error
	return nullptr;
}

ExpressionPtr Parser::FullNewExpression()
{
	if (Match(TokenType::New))
	{
		auto subExpr = FullNewSubexpression();
		auto arguments = Arguments();
	}
	return nullptr;
}

ExpressionPtr Parser::FullNewSubexpression()
{
	ExpressionPtr result;
	if (result = PrimaryExpression())
	{
		return result;
	}
	NOT_IMPLEMENTED;
	return nullptr;
}

ExpressionPtr Parser::Unary()
{
	if (MatchOneOf({
		TokenType::Delete,
		TokenType::MinusMinus,
		TokenType::PlusPlus,
		}))
	{
		auto op = Prev().Type;
		auto ls = LeftSideExpression();
		auto suffix = false;
		return IPLMakeSharePtr<UnaryExpression>(ls, op, suffix);
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
		auto type = Prev().Type;
		auto ls = Unary();
		auto suffix = false;
		return IPLMakeSharePtr<UnaryExpression>(ls, type, suffix);
	}
	else
	{
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
	auto left = RelationalExpression();
	while (MatchOneOf({TokenType::EqualEqual,
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
	auto BitwiseAndExpression = [&]() -> ExpressionPtr {
		auto left = EqualityExpression();
		while (Match(TokenType::BitwiseAnd))
		{
			auto type = Prev().Type;
			auto right = EqualityExpression();
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	auto BitwiseXorExpression = [&]() -> ExpressionPtr {
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
	BitwiseOrExpression = [&]() -> ExpressionPtr {
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
	LogicalAndExpression = [&]() -> ExpressionPtr {
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
	LogicalOrExpression = [&]() -> ExpressionPtr {
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
	auto location = GetLocation();
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
		auto type = Prev().Type;
		auto right = AssignmentExpression();
		auto be = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		if (be)
		{
			be->SetLocation(location.Line, location.Column);
		}
		return be;
	}
	// revert state;
	m_Current = snapShot;
	auto ce = ConditionalExpression();
	if (ce)
	{
		ce->SetLocation(location.Line, location.Column);
	}
	return ce;
}

ExpressionPtr Parser::Expression()
{
	auto ae = AssignmentExpression();
	while (Match(TokenType::Comma))
	{
		auto type = Prev().Type;
		auto next = AssignmentExpression();
		if (next)
		{
			ae = IPLMakeSharePtr<BinaryExpression>(ae, next, type);
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
	ExpressionPtr result;
	if (result = EmptyStatement()) return result;
	if (result = Expression()) return result;
	if (result = VariableDefinition()) return result;
	if (result = Block()) return result;
	if (result = LabeledStatement()) return result;
	if (result = IfStatementfull()) return result;
	if (result = SwitchStatement()) return result;
	if (result = DoStatement()) return result;
	if (result = WhileStatement()) return result;
	if (result = ForStatement()) return result;
	if (result = WithStatement()) return result;
	if (result = ContinueStatement()) return result;
	if (result = BreakStatement()) return result;
	if (result = OptionalLabel()) return result;
	if (result = ReturnStatement()) return result;
	if (result = TryStatement()) return result;
	return result;
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
	auto VariableDeclaration = [&]() -> ExpressionPtr {
		auto location = GetLocation();
		if (Match(TokenType::Identifier))
		{
			auto id = Prev().Lexeme;
			auto ae = CreateEmptyExpression();
			if (Match(TokenType::Equal))
			{
				ae = AssignmentExpression();
			}
			auto vd = IPLMakeSharePtr<VariableDefinitionExpression>(id, ae);
			vd->SetLocation(location.Line, location.Column);
			return vd;
		}
		return nullptr;
	};

	auto VariableDeclarationList = [&]() -> ExpressionPtr {
		auto vdList = IPLMakeSharePtr<ListExpression>();
		vdList->GetValuesByRef().push_back(VariableDeclaration());
		while (Match(TokenType::Comma))
		{
			vdList->GetValuesByRef().push_back(VariableDeclaration());
		}
		return vdList;
	};

	if (Match(TokenType::Var))
	{
		return VariableDeclarationList();
	}

	return nullptr;
}

ExpressionPtr Parser::Block()
{
	auto BlockStatementsPrefix = [&]() -> ExpressionPtr {
		auto StatementsList = IPLMakeSharePtr<BlockStatement>();
		while (auto s = Statement())
		{
			StatementsList->GetValuesByRef().push_back(s);
		}
		return StatementsList;
	};

	if (Match(TokenType::LeftBrace))
	{
		auto statements = BlockStatementsPrefix();
		if (Match(TokenType::RightBrace))
		{
			return statements;
		}
		else
		{
			// TODO log error - empty block?
			assert(false);
			return nullptr;
		}
	}
	return nullptr;
}

ExpressionPtr Parser::LabeledStatement()
{
	auto location = GetLocation();
	if (Match(TokenType::Identifier))
	{
		auto identifier = Prev().Lexeme;
		auto stament = Statement();
		auto ls = IPLMakeSharePtr<::LabeledStatement>(identifier, stament);
		ls->SetLocation(location.Line, location.Column);
		return ls;
	}
	return nullptr;
}

ExpressionPtr Parser::IfStatementfull()
{
	auto location = GetLocation();
	if (Match(TokenType::If))
	{
		auto cond = ParenthesizedExpression();
		auto ifBody = Statement();
		ExpressionPtr elseBody = nullptr;
		if (Match(TokenType::Else))
		{
			elseBody = Statement();
		}
		auto is = IPLMakeSharePtr<::IfStatement>(cond, ifBody, elseBody);
		is->SetLocation(location.Line, location.Column);
		return is;
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
	auto location = GetLocation();
	if (Match(TokenType::While))
	{
		auto cond = ParenthesizedExpression();
		auto body = Statement();
		auto isDoWhile = false;
		auto ws = IPLMakeSharePtr<::WhileStatement>(cond, body, isDoWhile);
		ws->SetLocation(location.Line, location.Column);
		return ws;
	}
	return nullptr;
}

ExpressionPtr Parser::ForStatement()
{
	auto location = GetLocation();
	if (Match(TokenType::For))
	{
		if (!Match(TokenType::LeftParen))
		{
			assert(false);
		}
		auto initializer = Expression();
		if (!initializer)
		{
			initializer = VariableDefinition();
		}
		if (!Match(TokenType::Semicolon))
		{
			assert(false);
		}

		auto cond = Expression();
		if (!Match(TokenType::Semicolon))
		{
			assert(false);
		}
		auto iteration = Expression();
		if (!Match(TokenType::RightParen))
		{
			assert(false);
		}
		auto body = Statement();
		auto fs = IPLMakeSharePtr<::ForStatement>(initializer, cond, iteration, body);
		fs->SetLocation(location.Line, location.Column);
		return fs;
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
	return Parser::Expression();
}

ExpressionPtr Parser::Arguments()
{
	if (Match(TokenType::LeftParen))
	{
		auto result = IPLMakeSharePtr<ListExpression>();
		auto current = AssignmentExpression();
		while (current)
		{
			result->GetValuesByRef().push_back(current);
			if (Match(TokenType::Comma))
			{
				current = AssignmentExpression();
				if (current)
				{
					// TODO error
					return nullptr;
				}
			}
			else
			{
				current = nullptr;
			}
		}

		if (Match(TokenType::RightParen))
		{
			return result;
		}
		// TODO error
		return nullptr;
	}
	return nullptr;
}

ExpressionPtr Parser::Parse()
{
	auto result = Parser::Program();
	assert(m_Current + 1 == m_Tokens.size());
	return result;
}

ExpressionPtr Parser::FunctionDefinition()
{
	auto  FormalParameters = [&](IPLVector<IPLString>& identifiers) -> bool {
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

	auto Body = [&]() -> ExpressionPtr {

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
	auto location = GetLocation();
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
					auto fd = IPLMakeSharePtr<FunctionDeclaration>(name, identifiers, body);
					fd->SetLocation(location.Line, location.Column);
					return fd;
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

Parser::InternalState Parser::Snapshot()
{
	return { m_Current };
}

void Parser::Restore(const InternalState& state)
{
	m_Current = state.Current;
}
