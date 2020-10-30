#include "Parser.h"

class Parser
{
public:
	Parser(const IPLVector<Token>& tokens, const std::function<void()>& onError = {});
	ExpressionPtr Parse();
private:
	enum NormalType {
		Initial,
		Normal,
	};

	enum AllowType {
		AllowIn,
		NoIn,
	};

	enum CompletenessType {
		NoShortIf,
		Full
	};

	bool MatchOneOf(IPLVector<TokenType> types);
	bool Match(TokenType type);
	ExpressionPtr RegularExpression();
	ExpressionPtr ParenthesizedExpression();
	ExpressionPtr PrimaryExpression(NormalType a);
	ExpressionPtr FunctionExpression();
	ExpressionPtr ObjectLiteral();
	ExpressionPtr ArrayLiteral();
	ExpressionPtr Unary(NormalType a);
	ExpressionPtr LeftSideExpression(NormalType a);
	ExpressionPtr CallExpression(NormalType a);
	ExpressionPtr CallExpressionHelper(NormalType a);
	ExpressionPtr ShortNewExpression();
	ExpressionPtr ShortNewSubexpression();
	ExpressionPtr FullNewExpression();
	ExpressionPtr FullNewSubexpression();
	ExpressionPtr SimpleExpression();
	ExpressionPtr MultiplicativeExpression(NormalType a);
	ExpressionPtr AdditiveExpression(NormalType a);
	ExpressionPtr ShiftExpression(NormalType a);
	ExpressionPtr RelationalExpression(NormalType a, AllowType b);
	ExpressionPtr EqualityExpression(NormalType a, AllowType b);
	ExpressionPtr BitwiseExpression(NormalType a, AllowType b);
	ExpressionPtr LogicalExpression(NormalType a, AllowType b);
	ExpressionPtr ConditionalExpression(NormalType a, AllowType b);
	ExpressionPtr AssignmentExpression(NormalType a, AllowType b);
	ExpressionPtr Expression(NormalType a, AllowType b);
	ExpressionPtr OptionalExpression();
	ExpressionPtr Arguments();

	//Statements
	ExpressionPtr Statement();
	ExpressionPtr EmptyStatement();
	ExpressionPtr VariableDefinition(AllowType b);
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

ExpressionPtr Parser::PrimaryExpression(NormalType a)
{
	if (a == NormalType::Initial)
	{
		return SimpleExpression();
	}

	if (auto simple = SimpleExpression())
	{
		return simple;
	}
	else if(auto function = FunctionExpression())
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
		auto ex = Parser::Expression(NormalType::Normal, AllowType::AllowIn);
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
	if (Match(TokenType::LeftBrace))
	{
		using FieldList = IPLVector<ExpressionPtr>;
		auto MatchLiteralField = [&]() ->ExpressionPtr {
			if (!Match(TokenType::Identifier))
			{
				return nullptr;
			}
			auto id = IPLMakeSharePtr<IdentifierExpression>(Prev().Lexeme);
			if (!Match(TokenType::Colon))
			{
				return nullptr;
			}
			auto ae = AssignmentExpression(NormalType::Normal, AllowType::AllowIn);
			return IPLMakeSharePtr<LiteralField>(id, ae);
		};

		auto MatchFieldList = [&](FieldList& result) -> bool {
			auto lf = MatchLiteralField();
			while (lf)
			{
				result.push_back(lf);
				if (!Match(TokenType::Comma))
				{
					break;
				}
				lf = MatchLiteralField();
				if (!lf)
				{
					// TODO log error
					return false;
				}
			}
			return true;
		};
		FieldList fl;
		if (!MatchFieldList(fl))
		{
			return nullptr;
		}
		auto ol = IPLMakeSharePtr<LiteralObject>(fl);
		if (Match(TokenType::RightBrace))
		{
			return ol;
		}
		// TODO Add error loging
		assert(false);
	}
	return nullptr;
}

ExpressionPtr Parser::ArrayLiteral()
{
	if (Match(TokenType::LeftSquareBracket))
	{
		auto LiteralElement = [&]() -> ExpressionPtr {
			return AssignmentExpression(NormalType::Normal, AllowType::AllowIn);
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

ExpressionPtr Parser::LeftSideExpression(NormalType a)
{
	ExpressionPtr result;
	auto ss = Snapshot();
	if (result = CallExpression(a))
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

ExpressionPtr Parser::CallExpression(NormalType a)
{
	ExpressionPtr result;
	auto ss = Snapshot();
	if (result = PrimaryExpression(a))
	{
		return result;
	}
	Restore(ss);
	if (result = FullNewExpression())
	{
		return result;
	}

	if (result = CallExpressionHelper(a))
	{
		return result;
	}
	// TODO error
	return nullptr;
}

ExpressionPtr Parser::CallExpressionHelper(NormalType a)
{
	if (Match(TokenType::LeftSquareBracket))
	{
		auto expr = Expression(NormalType::Normal, AllowType::AllowIn);
		if (Match(TokenType::RightSquareBracket))
		{
			// return  meaningfull expr
			auto next = CallExpressionHelper(a);
			return nullptr;
		}
		// TODO error
		return nullptr;
	}
	if (Match(TokenType::Dot))
	{
		if (Match(TokenType::Identifier))
		{
			auto next = CallExpressionHelper(a);
			// return  meaningfull expr
			return nullptr;
		}
		// TODO error
		return nullptr;
	}

	if (auto args = Arguments())
	{
		// return  meaningfull expr
		auto next = CallExpressionHelper(a);
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
	if (result = PrimaryExpression(NormalType::Normal))
	{
		return result;
	}
	NOT_IMPLEMENTED;
	return nullptr;
}

ExpressionPtr Parser::Unary(NormalType a)
{
	if (MatchOneOf({
		TokenType::Delete,
		TokenType::MinusMinus,
		TokenType::PlusPlus,
		}))
	{
		auto op = Prev().Type;
		auto ls = LeftSideExpression(NormalType::Normal);
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
		auto ls = Unary(NormalType::Normal);
		auto suffix = false;
		return IPLMakeSharePtr<UnaryExpression>(ls, type, suffix);
	}
	else
	{
		auto leftSide = LeftSideExpression(a);
		if (MatchOneOf({ TokenType::PlusPlus, TokenType::MinusMinus }))
		{
			auto suffix = true;
			return IPLMakeSharePtr<UnaryExpression>(leftSide, Prev().Type, suffix);
		}
		return leftSide;
	}
}

ExpressionPtr Parser::MultiplicativeExpression(NormalType a)
{
	auto left = Unary(a);

	while(MatchOneOf({ TokenType::Star, TokenType::Division, TokenType::Modulo }))
	{
		auto type = Prev().Type;
		auto right = Unary(a);
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
	}
	return left;
}

ExpressionPtr Parser::AdditiveExpression(NormalType a)
{
	auto left = MultiplicativeExpression(a);
	while (MatchOneOf({ TokenType::Plus, TokenType::Minus}))
	{
		auto type = Prev().Type;
		auto right = MultiplicativeExpression(a);
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
	}
	return left;
}

ExpressionPtr Parser::ShiftExpression(NormalType a)
{
	auto left = AdditiveExpression(a);
	while (MatchOneOf({ TokenType::LeftShift, TokenType::RightShift }))
	{
		auto type = Prev().Type;
		auto right = AdditiveExpression(a);
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
	}
	return left;
}

ExpressionPtr Parser::RelationalExpression(NormalType a, AllowType)
{
	auto left = ShiftExpression(a);
	while (MatchOneOf({ TokenType::Less,
		TokenType::Greater,
		TokenType::LessEqual,
		TokenType::GreaterEqual,
		TokenType::Instanceof,
		TokenType::In
	}))
	{
		auto type = Prev().Type;
		auto right = ShiftExpression(a);
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
	}
	return left;
}

ExpressionPtr Parser::EqualityExpression(NormalType a, AllowType b)
{
	auto left = RelationalExpression(a, b);
	while (MatchOneOf({TokenType::EqualEqual,
		TokenType::BangEqual,
		TokenType::StrictEqual,
		TokenType::StrictNotEqual,

	}))
	{
		auto type = Prev().Type;
		auto right = RelationalExpression(a, b);
		left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
	}
	return left;
}

ExpressionPtr Parser::BitwiseExpression(NormalType a, AllowType b)
{
	auto BitwiseAndExpression = [&](NormalType _a, AllowType _b) -> ExpressionPtr {
		auto left = EqualityExpression(_a, _b);
		while (Match(TokenType::BitwiseAnd))
		{
			auto type = Prev().Type;
			auto right = EqualityExpression(_a, _b);
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	auto BitwiseXorExpression = [&](NormalType _a, AllowType _b) -> ExpressionPtr {
		auto left = BitwiseAndExpression(_a, _b);
		while (Match(TokenType::BitwiseXor))
		{
			auto type = Prev().Type;
			auto right = BitwiseAndExpression(_a, _b);
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	auto BitwiseOrExpression = [&](NormalType _a, AllowType _b) -> ExpressionPtr {
		auto left = BitwiseXorExpression(_a, _b);
		while (Match(TokenType::BitwiseOr))
		{
			auto type = Prev().Type;
			auto right = BitwiseXorExpression(_a, _b);
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	return BitwiseOrExpression(a, b);
}


ExpressionPtr Parser::LogicalExpression(NormalType a, AllowType b)
{
	std::function<ExpressionPtr(NormalType a, AllowType b)> LogicalAndExpression;
	LogicalAndExpression = [&](NormalType a, AllowType b) -> ExpressionPtr {
		auto left = BitwiseExpression(a, b);
		while (Match(TokenType::LogicalAnd))
		{
			auto type = Prev().Type;
			auto right = BitwiseExpression(a, b);
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	auto LogicalOrExpression = [&](NormalType a, AllowType b) -> ExpressionPtr {
		auto left = LogicalAndExpression(a, b);
		while (Match(TokenType::LogicalAnd))
		{
			auto type = Prev().Type;
			auto right = LogicalAndExpression(a, b);
			left = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		}
		return left;
	};

	return LogicalOrExpression(a, b);
}

ExpressionPtr Parser::ConditionalExpression(NormalType a, AllowType b)
{
	auto condition = LogicalExpression(a, b);
	if (Match(TokenType::QuestionMark))
	{
		auto trueExpr= AssignmentExpression(NormalType::Normal, b);
		if (Match(TokenType::Colon))
		{
			auto falseExpr = AssignmentExpression(NormalType::Normal, b);
			// TODO implement trinary operator
			assert(false);
			return ExpressionPtr();
		}
		// TODO log parsing error
		assert(false);
	}
	return condition;
}

ExpressionPtr Parser::AssignmentExpression(NormalType a, AllowType b)
{
	auto location = GetLocation();

	auto left = ConditionalExpression(a, b);
	if (!left)
	{
		left = LeftSideExpression(a);
	}

	if (!left)
	{
		return nullptr;
	}
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
		auto right = AssignmentExpression(NormalType::Normal, b);
		auto be = IPLMakeSharePtr<BinaryExpression>(left, right, type);
		if (be)
		{
			be->SetLocation(location.Line, location.Column);
		}
		return be;
	}

	return left;
}

ExpressionPtr Parser::Expression(NormalType a, AllowType b)
{
	auto ae = AssignmentExpression(a, b);
	while (Match(TokenType::Comma))
	{
		auto type = Prev().Type;

		auto next = AssignmentExpression(a, b);
		if (!next)
		{
			next = AssignmentExpression(NormalType::Normal, b);
		}

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
	if (result = Expression(NormalType::Initial, AllowType::AllowIn)) return result;
	if (result = VariableDefinition(AllowType::AllowIn)) return result;
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

ExpressionPtr Parser::VariableDefinition(AllowType b)
{
	auto VariableDeclaration = [&]() -> ExpressionPtr {
		auto location = GetLocation();
		if (Match(TokenType::Identifier))
		{
			auto id = Prev().Lexeme;
			auto ae = CreateEmptyExpression();
			if (Match(TokenType::Equal))
			{
				ae = AssignmentExpression(NormalType::Normal, b);
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
					auto expr = Expression(NormalType::Normal, AllowType::AllowIn);
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
		auto initializer = Expression(NormalType::Normal, AllowType::AllowIn);
		if (!initializer)
		{
			initializer = VariableDefinition(AllowType::AllowIn);
		}
		if (!Match(TokenType::Semicolon))
		{
			assert(false);
		}

		auto cond = OptionalExpression();
		if (!Match(TokenType::Semicolon))
		{
			assert(false);
		}
		auto iteration = OptionalExpression();
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
	return Parser::Expression(NormalType::Normal, AllowType::AllowIn);
}

ExpressionPtr Parser::Arguments()
{
	if (Match(TokenType::LeftParen))
	{
		auto result = IPLMakeSharePtr<ListExpression>();
		auto current = AssignmentExpression(NormalType::Normal, AllowType::AllowIn);
		while (current)
		{
			result->GetValuesByRef().push_back(current);
			if (Match(TokenType::Comma))
			{
				current = AssignmentExpression(NormalType::Normal, AllowType::AllowIn);
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
