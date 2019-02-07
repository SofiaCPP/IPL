#include "ASTInterpreter.h"
#include "Expression.h"
#include <iostream>
#include <math.h>

#undef NOT_IMPLEMENTED
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

    virtual void Visit(IdentifierExpression* e) override { m_LValue = &m_Interpreter->ModifyVariable(e->GetName()); }

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

struct StackScope
{
    StackScope(ASTInterpreter* interpreter)
        : m_Interpreter(interpreter)
                , m_StackSize(m_Interpreter->m_Evaluation.size())
    {
    }

    ~StackScope() {
        m_Interpreter->m_Evaluation.resize(m_StackSize);
    }

    StackScope(StackScope&) = delete;
    StackScope(StackScope&&) = delete;
    StackScope& operator=(StackScope&) = delete;
    StackScope& operator=(StackScope&&) = delete;

    ASTInterpreter* m_Interpreter;
        size_t m_StackSize;
};

ASTInterpreter::ASTInterpreter()
    : m_Fallthrough(false)
    , m_Break(false)
    , m_Continue(false)
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
    auto result = m_Evaluation.back();
    m_Evaluation.pop_back();
    return result != 0.0;
}

bool ASTInterpreter::EvalIsEqual(const ExpressionPtr& e)
{
    e->Accept(*this);
    auto second = m_Evaluation.back();
    m_Evaluation.pop_back();
    auto first = m_Evaluation.back();
    return fabs(second - first) < 0.0001;
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
        values->second.pop_back();
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
        return values->second.back();
    }
    else {
        assert(0 && "not-defined");
    }
    static value_type undefined;
    return undefined;
}

bool ASTInterpreter::HasVariable(const IPLString& name)
{
    return m_Variables.find(name) != m_Variables.end();
}

bool ASTInterpreter::IsInSkipMode()
{
    return m_Break || m_Continue;
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

void ASTInterpreter::Visit(LiteralObject* e)
{
   NOT_IMPLEMENTED;
}

void ASTInterpreter::Visit(LiteralNumber* e) {
    m_Evaluation.push_back(e->GetValue());
}
void ASTInterpreter::Visit(LiteralBoolean* e) {
    m_Evaluation.push_back(e->GetValue());
}

void ASTInterpreter::Visit(BinaryExpression* e) {
    e->GetLeft()->Accept(*this);
    e->GetRight()->Accept(*this);
    auto right = m_Evaluation.back();
    m_Evaluation.pop_back();

    auto left = m_Evaluation.back();
    m_Evaluation.pop_back();

    switch (e->GetOperator()) {
        case TokenType::Plus:
            m_Evaluation.push_back(left + right);
            break;
        case TokenType::Minus:
            m_Evaluation.push_back(left - right);
            break;
        case TokenType::Star:
            m_Evaluation.push_back(left * right);
            break;
        case TokenType::Division:
            m_Evaluation.push_back(left / right);
            break;
        case TokenType::Less:
            m_Evaluation.push_back(left < right);
            break;
		case TokenType::LessEqual:
			m_Evaluation.push_back(left <= right);
			break;
		case TokenType::Greater:
			m_Evaluation.push_back(left > right);
			break;
		case TokenType::GreaterEqual:
			m_Evaluation.push_back(left >= right);
			break;
        case TokenType::Comma:
            m_Evaluation.push_back(right);
            break;
        case TokenType::EqualEqual:
            m_Evaluation.push_back(fabs(left - right) < 0.0001);
            break;
        case TokenType::BangEqual:
            m_Evaluation.push_back(fabs(left - right) > 0.0001);
            break;
        case TokenType::Equal:
        {
            LValueExtractor extractor(this);
            auto lvalue = extractor.Run(e->GetLeft().get());
            *lvalue = right;
        }
        break;
        default:
            NOT_IMPLEMENTED;
    }
}

void ASTInterpreter::Visit(UnaryExpression* e) {
    switch (e->GetOperator())
    {
    case TokenType::Break:
        m_Break = true;
        return;
    case TokenType::Continue:
        m_Continue = true;
        return;
    default:
        ;
    }

    LValueExtractor extractor(this);
    auto lvalue = extractor.Run(e->GetExpr().get());
    if (e->GetSuffix())
    {
        m_Evaluation.push_back(*lvalue);
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
        m_Evaluation.push_back(*lvalue);
    }
}

void ASTInterpreter::Visit(IdentifierExpression* e) {
    m_Evaluation.push_back(ModifyVariable(e->GetName()));
}

void ASTInterpreter::Visit(ListExpression* e) {
    for (const auto& stmt: e->GetValues()) {
        stmt->Accept(*this);
    }
}

void ASTInterpreter::Visit(VariableDefinitionExpression* e) {
    e->GetValue()->Accept(*this);
    auto& name = e->GetName();
    m_Variables[name].push_back(m_Evaluation.back());
    m_Scopes.top().push_back(name);
    m_Evaluation.pop_back();
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

void ASTInterpreter::Visit(SwitchStatement* e)
{
    VariableScope scope(this);
    e->GetCondition()->Accept(*this);
    for (const auto& stmt : e->GetCases())
        stmt->Accept(*this);
    RunExpression(e->GetDefaultCase());
    m_Evaluation.pop_back();

    m_Fallthrough = false;
    m_Break = false;
}

void ASTInterpreter::Visit(CaseStatement *e)
{
    if (m_Break)
        return;

    if (m_Fallthrough || EvalIsEqual(e->GetCondition()))
    {
        RunExpression(e->GetBody());
        m_Fallthrough = true;
    }
}

void ASTInterpreter::Visit(WhileStatement* e) {
    VariableScope scope(this);
    if (e->GetDoWhile()) {
        VariableScope bodyScope(this);
        RunExpression(e->GetBody());
        m_Continue = false;
    }
    while (EvalToBool(e->GetCondition())) {
        VariableScope bodyScope(this);
        RunExpression(e->GetBody());
        m_Continue = false;
    }

    m_Break = false;
}

void ASTInterpreter::Visit(ForStatement* e) {
    VariableScope scope(this);

    {
        StackScope stackScope(this);
        RunExpression(e->GetInitialization());
    }
    while (EvalToBool(e->GetCondition()))
    {
        {
            VariableScope bodyScope(this);
            RunExpression(e->GetBody());
            m_Continue = false;
        }
        {
            StackScope stackScope(this);
            RunExpression(e->GetIteration());
        }
    }

    m_Break = false;
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

void ASTInterpreter::Visit(CallExpression* e)
{
    NOT_IMPLEMENTED;
}

void ASTInterpreter::Print(Printer& p)
{
    for (auto& it : m_Variables)
    {
        p.PrintVariable(it.first.c_str(), ModifyVariable(it.first));
    }
}