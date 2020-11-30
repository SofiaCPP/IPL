#include "ASTInterpreter.h"
#include "Expression.h"
#include <iostream>
#include <math.h>

#undef NOT_IMPLEMENTED
#define NOT_IMPLEMENTED (void)e; assert(0 && "not-implemented")

namespace
{
    template <typename T>
    ASTInterpreter::ValuePtr MakeValue(T&& v)
    {
        return IPLMakeSharePtr<ASTInterpreter::Value>(v);
    }

    template <typename T>
    ASTInterpreter::ValuePtr MakeValue(const T& v)
    {
        return IPLMakeSharePtr<ASTInterpreter::Value>(v);
    }

    struct ValueToBool
    {
        bool operator()(double d) const
        {
            return d != 0.0;
        }
        bool operator()(const IPLString& s) const
        {
            return !s.empty();
        }
    };

    struct UnsupportedOpReport
    {
        template <typename V>
        ASTInterpreter::ValuePtr operator()(V) const
        {
            throw std::runtime_error("unsupported operation");
        }

        template <typename L, typename R>
        ASTInterpreter::ValuePtr operator()(L, R) const
        {
            throw std::runtime_error("unsupported operation");
        }
    };

    struct ValueAdd : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        ASTInterpreter::ValuePtr operator()(double l, double r) const
        {
            return MakeValue(l + r);
        }

        ASTInterpreter::ValuePtr operator()(const IPLString& l, const IPLString& r) const
        {
            return MakeValue(l + r);
        }
    };

    struct ValueMinus : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        ASTInterpreter::ValuePtr operator()(double l, double r) const
        {
            return MakeValue(l - r);
        }
    };

    struct ValueProduct : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        ASTInterpreter::ValuePtr operator()(double l, double r) const
        {
            return MakeValue(l * r);
        }
    };

    struct ValueDivide : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        ASTInterpreter::ValuePtr operator()(double l, double r) const
        {
            return MakeValue(l / r);
        }
    };

    struct ValueLess : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        template <typename V>
        ASTInterpreter::ValuePtr operator()(const V& l, const V& r) const
        {
            return MakeValue(l < r);
        }
    };
    struct ValueLessEqual : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        template <typename V>
        ASTInterpreter::ValuePtr operator()(V l, V r) const
        {
            return MakeValue(l <= r);
        }
    };

    struct ValueGreater : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        template <typename V>
        ASTInterpreter::ValuePtr operator()(V l, V r) const
        {
            return MakeValue(l > r);
        }
    };

    struct ValueGreaterEqual : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        template <typename V>
        ASTInterpreter::ValuePtr operator()(V l, V r) const
        {
            return MakeValue(l >= r);
        }
    };

    struct ValueEqualEqual : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        ASTInterpreter::ValuePtr operator()(double left, double right) const
        {
            return MakeValue(fabs(left - right) <= 0.0001);
        }

        ASTInterpreter::ValuePtr operator()(const IPLString& left, const IPLString& right) const
        {
            return MakeValue(left == right);
        }
    };

    struct ValueBangEqual : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        ASTInterpreter::ValuePtr operator()(double left, double right) const
        {
            return MakeValue(fabs(left - right) > 0.0001);
        }
        ASTInterpreter::ValuePtr operator()(const IPLString& left, const IPLString& right) const
        {
            return MakeValue(left != right);
        }
    };

    struct ValueUnaryMinus : UnsupportedOpReport
    {
        using UnsupportedOpReport::operator();

        ASTInterpreter::ValuePtr operator()(double v) const
        {
            return MakeValue(-v);
        }
    };

    struct ValueIncrement
    {
        void operator()(double& v) const
        {
            v += 1;
        }

        template <typename V>
        void operator()(V&) const
        {
            throw std::runtime_error("unsupported operation");
        }
    };

    struct ValueDecrement
    {
        void operator()(double& v) const
        {
            v -= 1;
        }

        template <typename V>
        void operator()(V&) const
        {
            throw std::runtime_error("unsupported operation");
        }
    };

    struct ValuePrint
    {
        ValuePrint(const char* name, ASTInterpreter::Printer* printer)
            : Name(name)
            , Printer(printer)
        {}

        void operator()(double value) const
        {
            Printer->PrintVariable(Name, value);
        }

        void operator()(const IPLString& value) const
        {
            Printer->PrintVariable(Name, value);
        }

        const char* Name;
        ASTInterpreter::Printer* Printer;
    };
}

struct LValueExtractor : public ExpressionVisitor
{
public:
    LValueExtractor(ASTInterpreter* interpreter)
        : m_Interpreter(interpreter)
        , m_LValue(nullptr)
    {}
    ~LValueExtractor() {}

    ASTInterpreter::ValuePtr Run(Expression* program)
    {
        m_LValue = nullptr;
        program->Accept(*this);
        return m_LValue;
    }

    virtual void Visit(IdentifierExpression* e) override { m_LValue = m_Interpreter->ModifyVariable(e->GetName()); }
    virtual void Visit(Call* e) override { e->GetObjectOrCall()->Accept(*this); }

    ASTInterpreter* m_Interpreter;
    ASTInterpreter::ValuePtr m_LValue;
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
    return std::visit(ValueToBool(), *result);
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

ASTInterpreter::ValuePtr& ASTInterpreter::ModifyVariable(const IPLString& name)
{
    auto values = m_Variables.find(name);
    if (values != m_Variables.end()) {
        return values->second.back();
    }
    else {
        assert(0 && "not-defined");
    }
    static ValuePtr undefined;
    return undefined;
}

bool ASTInterpreter::HasVariable(const IPLString& name)
{
    return m_Variables.find(name) != m_Variables.end();
}

void ASTInterpreter::Visit(LiteralNull* e) {
   NOT_IMPLEMENTED; 
}

void ASTInterpreter::Visit(LiteralUndefined* e) {
   NOT_IMPLEMENTED; 
}
void ASTInterpreter::Visit(LiteralString* e) {
    m_Evaluation.push_back(MakeValue(e->GetValue()));
}

void ASTInterpreter::Visit(LiteralObject* e)
{
   NOT_IMPLEMENTED;
}

void ASTInterpreter::Visit(LiteralNumber* e) {
    m_Evaluation.push_back(MakeValue(e->GetValue()));
}
void ASTInterpreter::Visit(LiteralBoolean* e) {
    m_Evaluation.push_back(MakeValue(e->GetValue()));
}

void ASTInterpreter::Visit(BinaryExpression* e) {
    e->GetLeft()->Accept(*this);
    e->GetRight()->Accept(*this);
    auto right = *m_Evaluation.back();
    m_Evaluation.pop_back();

    auto left = *m_Evaluation.back();
    m_Evaluation.pop_back();

    switch (e->GetOperator()) {
        case TokenType::Plus:
            m_Evaluation.push_back(std::visit(ValueAdd(), left, right));
            break;
        case TokenType::Minus:
            m_Evaluation.push_back(std::visit(ValueMinus(), left, right));
            break;
        case TokenType::Star:
            m_Evaluation.push_back(std::visit(ValueProduct(), left, right));
            break;
        case TokenType::Division:
            m_Evaluation.push_back(std::visit(ValueDivide(), left, right));
            break;
        case TokenType::Less:
            m_Evaluation.push_back(std::visit(ValueLess(), left, right));
            break;
        case TokenType::LessEqual:
            m_Evaluation.push_back(std::visit(ValueLessEqual(), left, right));
            break;
        case TokenType::Greater:
            m_Evaluation.push_back(std::visit(ValueGreater(), left, right));
            break;
        case TokenType::GreaterEqual:
            m_Evaluation.push_back(std::visit(ValueGreaterEqual(), left, right));
            break;
        case TokenType::Comma:
            m_Evaluation.push_back(MakeValue(std::move(right)));
            break;
        case TokenType::EqualEqual:
            m_Evaluation.push_back(std::visit(ValueEqualEqual(), left, right));
            break;
        case TokenType::BangEqual:
            m_Evaluation.push_back(std::visit(ValueBangEqual(), left, right));
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
        case TokenType::Minus:
        {
            e->GetExpr()->Accept(*this);
            auto v = m_Evaluation.back();
            m_Evaluation.pop_back();
            m_Evaluation.push_back(std::visit(ValueUnaryMinus(), *v));
            break;
        }
        case TokenType::Plus:
        {
            e->GetExpr()->Accept(*this);
            break;
        }
        case TokenType::PlusPlus: // FALLTHROUGH
        case TokenType::MinusMinus:
        {
            LValueExtractor extractor(this);
            auto lvalue = extractor.Run(e->GetExpr().get());
            if (e->GetSuffix())
            {
                m_Evaluation.push_back(MakeValue(*lvalue));
            }
            if (e->GetOperator() == TokenType::PlusPlus)
            {
                std::visit(ValueIncrement(), *lvalue);
            }
            else
            {
                std::visit(ValueDecrement(), *lvalue);
            }
            if (!e->GetSuffix())
            {
                m_Evaluation.push_back(MakeValue(*lvalue));
            }
            break;
        }
        default:
        {
            NOT_IMPLEMENTED;
        }
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

    {
        StackScope stackScope(this);
        RunExpression(e->GetInitialization());
    }
    while (EvalToBool(e->GetCondition()))
    {
        {
            VariableScope bodyScope(this);
            RunExpression(e->GetBody());
        }
        {
            StackScope stackScope(this);
            RunExpression(e->GetIteration());
        }
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

void ASTInterpreter::Visit(Call* e)
{
    e->GetObjectOrCall()->Accept(*this);
    if (e->GetMember())
    {
        e->GetMember()->Accept(*this);
    }
}

void ASTInterpreter::Print(Printer& p)
{
    for (auto& it : m_Variables)
    {
        auto& value = ModifyVariable(it.first);
        std::visit(ValuePrint(it.first.c_str(), &p), *value);
    }
}
