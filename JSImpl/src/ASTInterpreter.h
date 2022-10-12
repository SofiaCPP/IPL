#pragma once

#include "ExpressionVisitor.h"

#include <variant>

class ASTInterpreter : public ExpressionVisitor
{
public:
    ASTInterpreter();
    ~ASTInterpreter();


    typedef std::variant<bool, double, IPLString> Value;
    typedef IPLSharedPtr<Value> ValuePtr;
    typedef IPLVector<ValuePtr> ValueStack;

    ValueStack Run(Expression* program);

    virtual void Visit(LiteralNull* e) override;
    virtual void Visit(LiteralUndefined* e) override;
    virtual void Visit(LiteralString* e) override;
    virtual void Visit(LiteralNumber* e) override;
    virtual void Visit(LiteralBoolean* e) override;
    virtual void Visit(LiteralObject* e) override;
    virtual void Visit(BinaryExpression* e) override;
    virtual void Visit(UnaryExpression* e) override;
    virtual void Visit(IdentifierExpression* e) override;
    virtual void Visit(ListExpression* e) override;
    virtual void Visit(VariableDefinitionExpression* e) override;
    virtual void Visit(BlockStatement* e) override;
    virtual void Visit(LabeledStatement* e) override;
    virtual void Visit(IfStatement* e) override;
    virtual void Visit(SwitchStatement* e) override;
    virtual void Visit(CaseStatement *) override;
    virtual void Visit(WhileStatement* e) override;
    virtual void Visit(ForStatement* e) override;
    virtual void Visit(FunctionDeclaration* e) override;
    virtual void Visit(TopStatements* e) override;
    virtual void Visit(EmptyExpression* e) override;
    virtual void Visit(Call* e) override;

    ValuePtr& ModifyVariable(const IPLString& name);
    bool HasVariable(const IPLString& name);

	class Printer
	{
	public:
		virtual void PrintVariable(const char* name, double value) = 0;
		virtual void PrintVariable(const char* name, const IPLString& value) = 0;
	};

    void Print(Printer& p);
private:
    void RunExpression(const ExpressionPtr& e);
    bool EvalToBool(const ExpressionPtr& e);

    void EnterScope();
    void LeaveScope();
    friend struct VariableScope;
    friend struct StackScope;;

    ValueStack m_Evaluation;
    IPLUnorderedMap<IPLString, ValueStack> m_Variables;

    typedef IPLVector<IPLString> Scope;
    IPLStack<Scope> m_Scopes;
};
