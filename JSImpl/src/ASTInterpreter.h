#pragma once

#include "ExpressionVisitor.h"

class ASTInterpreter : public ExpressionVisitor
{
public:
    ASTInterpreter();
    ~ASTInterpreter();


    // TODO: Changing this to a shared_ptr<Value> (or GC-enabled value*) will
    // allow for much simpler implementation of all varible assignments.
    typedef double value_type;
    typedef IPLVector<value_type> ValueStack;

    ValueStack Run(Expression* program);

	virtual bool IsInSkipMode() override;

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
    virtual void Visit(CallExpression* e) override;

    value_type& ModifyVariable(const IPLString& name);
    bool HasVariable(const IPLString& name);

	class Printer
	{
	public:
		virtual void PrintVariable(const char* name, double value) = 0;
	};

    void Print(Printer& p);
private:
    void RunExpression(const ExpressionPtr& e);
    bool EvalToBool(const ExpressionPtr& e);
    bool EvalIsEqual(const ExpressionPtr& e);

    void EnterScope();
    void LeaveScope();
    friend struct VariableScope;
    friend struct StackScope;;

    ValueStack m_Evaluation;
    IPLUnorderedMap<IPLString, ValueStack> m_Variables;

    typedef IPLVector<IPLString> Scope;
    IPLStack<Scope> m_Scopes;

    bool m_Fallthrough;
    bool m_Break;
    bool m_Continue;
};
