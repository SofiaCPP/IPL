#include "ASTInterpreter.h"
#include "Expression.h"
#include <cassert>
#include <iostream>


#define NOT_IMPLEMENTED (void)e; assert(0 && "not-implemented")


struct LValueExtractor : public ExpressionVisitor
{
public:
	LValueExtractor(ASTInterpreter* interpreter)
		: m_Interpreter(interpreter)
	{}
	~LValueExtractor() {}

	ASTInterpreter::value_type* Run(Expression* program)
	{
		m_LValue = nullptr;
		program->Accept(*this);
		return m_LValue;
	}

	virtual void Visit(LiteralNull* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(LiteralUndefined* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(LiteralString* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(LiteralNumber* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(LiteralBoolean* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(BinaryExpression* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(UnaryExpression* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(IdentifierExpression* e) override { m_LValue = &m_Interpreter->ModifyVariable(e->GetName()); }
	virtual void Visit(ListExpression* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(VariableDefinitionExpression* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(BlockStatement* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(LabeledStatement* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(IfStatement* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(SwitchStatement* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(CaseStatement *e) override { NOT_IMPLEMENTED; }
	virtual void Visit(WhileStatement* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(ForStatement* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(FunctionDeclaration* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(TopStatements* e) override { NOT_IMPLEMENTED; }
	virtual void Visit(EmptyExpression* e) override { NOT_IMPLEMENTED; }

	ASTInterpreter* m_Interpreter;
	ASTInterpreter::value_type* m_LValue;
};


struct VariableScope
{
	VariableScope(ASTInterpreter* interpreter)
		: m_Interpreter(interpreter)
	{
		m_Interpreter->EnterScope();
	}

	~VariableScope() {
		m_Interpreter->LeaveScope();
	}

	VariableScope(VariableScope&) = delete;
	VariableScope(VariableScope&&) = delete;
	VariableScope& operator=(VariableScope&) = delete;
	VariableScope& operator=(VariableScope&&) = delete;

	ASTInterpreter* m_Interpreter;
};

ASTInterpreter::ASTInterpreter()
{
	// Allow globals
	EnterScope();
}

ASTInterpreter::~ASTInterpreter()
{}

ASTInterpreter::ValueStack ASTInterpreter::Run(Expression* program)
{
    program->Accept(*this);
    ValueStack result;
    m_Evaluation.swap(result);
    return result;
}

void ASTInterpreter::RunExpression(const ExpressionPtr& e)
{
    if (e) {
        e->Accept(*this);
    }
}

bool ASTInterpreter::EvalToBool(const ExpressionPtr& e)
{
    e->Accept(*this);
    auto result = m_Evaluation.top();
    m_Evaluation.pop();
    return result != 0.0;
}

void ASTInterpreter::EnterScope()
{
	m_Scopes.push(Scope{});
}

void ASTInterpreter::LeaveScope()
{
	const auto& scope = m_Scopes.top();
	for (auto& var : scope) {
		auto values = m_Variables.find(var);
		values->second.pop();
		if (values->second.empty()) {
			m_Variables.erase(values);
		}
	}
	m_Scopes.pop();
}

ASTInterpreter::value_type& ASTInterpreter::ModifyVariable(const IPLString& name)
{
	auto values = m_Variables.find(name);
	if (values != m_Variables.end()) {
		return values->second.top();
	}
	else {
		assert(0 && "not-defined");
	}
	static value_type undefined;
	return undefined;
}

void ASTInterpreter::Visit(LiteralNull* e) {
   NOT_IMPLEMENTED; 
}

void ASTInterpreter::Visit(LiteralUndefined* e) {
   NOT_IMPLEMENTED; 
}
void ASTInterpreter::Visit(LiteralString* e) {
   NOT_IMPLEMENTED; 
}

void ASTInterpreter::Visit(LiteralNumber* e) {
    m_Evaluation.push(e->GetValue());
}
void ASTInterpreter::Visit(LiteralBoolean* e) {
    m_Evaluation.push(e->GetValue());
}

void ASTInterpreter::Visit(BinaryExpression* e) {
    e->GetLeft()->Accept(*this);
    e->GetRight()->Accept(*this);
    auto right = m_Evaluation.top();
    m_Evaluation.pop();

    auto left = m_Evaluation.top();
    m_Evaluation.pop();

    switch (e->GetOperator()) {
        case TokenType::Plus:
            std::cout << "PLUS:" << left << ':' << right << std::endl;
            m_Evaluation.push(left + right);
            break;
        case TokenType::Minus:
            m_Evaluation.push(left - right);
            break;
        case TokenType::Star:
            m_Evaluation.push(left * right);
            break;
        case TokenType::Division:
            m_Evaluation.push(left / right);
            break;
	    case TokenType::Less:
			m_Evaluation.push(left < right);
			break;
		case TokenType::Comma:
			m_Evaluation.push(right);
			break;
        default:
            NOT_IMPLEMENTED;
    }
}

void ASTInterpreter::Visit(UnaryExpression* e) {
	LValueExtractor extractor(this);
	auto lvalue = extractor.Run(e->GetExpr().get());
	if (e->GetSuffix())
	{
		m_Evaluation.push(*lvalue);
	}
	switch (e->GetOperator())
	{
	case TokenType::PlusPlus:
		++(*lvalue);
		break;
	case TokenType::MinusMinus:
		--(*lvalue);
		break;
	default:
		NOT_IMPLEMENTED;
	}
	if (!e->GetSuffix())
	{
		m_Evaluation.push(*lvalue);
	}
}

void ASTInterpreter::Visit(IdentifierExpression* e) {
	m_Evaluation.push(ModifyVariable(e->GetName()));
}

void ASTInterpreter::Visit(ListExpression* e) {
    for (const auto& stmt: e->GetValues()) {
        stmt->Accept(*this);
    }
}

void ASTInterpreter::Visit(VariableDefinitionExpression* e) {
    e->GetValue()->Accept(*this);
	auto& name = e->GetName();
    m_Variables[name].push(m_Evaluation.top());
	m_Scopes.top().push_back(name);
    m_Evaluation.pop();
}

void ASTInterpreter::Visit(BlockStatement* e) {
    for (const auto& stmt: e->GetValues()) {
        stmt->Accept(*this);
    }
}

void ASTInterpreter::Visit(LabeledStatement* e) {
    RunExpression(e->GetStatement());
}

void ASTInterpreter::Visit(IfStatement* e) {
	VariableScope scope(this);
	if (EvalToBool(e->GetCondition())) {
		VariableScope ifScope(this);
		RunExpression(e->GetIfStatement());
    } else {
		VariableScope elseScope(this);
		RunExpression(e->GetElseStatement());
    }
}

void ASTInterpreter::Visit(SwitchStatement* e) { NOT_IMPLEMENTED;}
void ASTInterpreter::Visit(CaseStatement *e) { NOT_IMPLEMENTED;}

void ASTInterpreter::Visit(WhileStatement* e) {
	VariableScope scope(this);
    if (e->GetDoWhile()) {
		VariableScope bodyScope(this);
        RunExpression(e->GetBody());
    }
    while (EvalToBool(e->GetCondition())) {
		VariableScope bodyScope(this);
        RunExpression(e->GetBody());
    }
}

void ASTInterpreter::Visit(ForStatement* e) {
	VariableScope scope(this);

    for (RunExpression(e->GetInitialization());
            EvalToBool(e->GetCondition());
            RunExpression(e->GetIteration())) {
		VariableScope bodyScope(this);
        RunExpression(e->GetBody());
    }
}

void ASTInterpreter::Visit(FunctionDeclaration* e) { NOT_IMPLEMENTED;}
void ASTInterpreter::Visit(TopStatements* e) {
    for (const auto& stmt: e->GetValues()) {
        stmt->Accept(*this);
    }
}

void ASTInterpreter::Visit(EmptyExpression* e) {
    NOT_IMPLEMENTED;
}
