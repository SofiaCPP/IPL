#pragma once

#include "ExpressionVisitor.h"
#include <stack>
#include <string>
#include <unordered_map>

class ASTInterpreter : public ExpressionVisitor
{
public:
	ASTInterpreter();
	~ASTInterpreter();

        typedef double value_type;
        typedef std::stack<value_type> ValueStack;

        ValueStack Run(Expression* program);

	virtual void Visit(LiteralNull* e) override;
	virtual void Visit(LiteralUndefined* e) override;
	virtual void Visit(LiteralString* e) override;
	virtual void Visit(LiteralNumber* e) override;
	virtual void Visit(LiteralBoolean* e) override;
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
private:
        void RunExpression(const ExpressionPtr& e);
        bool EvalToBool(const ExpressionPtr& e);
        ValueStack m_Evaluation;
        std::unordered_map<std::string, ValueStack> m_Variables;
};
