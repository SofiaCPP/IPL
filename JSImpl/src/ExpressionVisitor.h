#pragma once

#include "ExpressionsFwd.h"

class ExpressionVisitor
{
public:
	~ExpressionVisitor() {}
	virtual void Visit(BinaryExpression* e) = 0;
	virtual void Visit(UnaryExpression* e) = 0;
	virtual void Visit(LiteralExpression* e) = 0;
	virtual void Visit(IdentifierExpression* e) = 0;
	virtual void Visit(ListExpression* e) = 0;
	virtual void Visit(VariableDefinitionExpression* e) = 0;
	virtual void Visit(BlockStatement* e) = 0;
	virtual void Visit(LabeledStatement* e) = 0;
	virtual void Visit(IfStatement* e) = 0;
	virtual void Visit(SwitchStatement* e) = 0;
	virtual void Visit(CaseStatement* e) = 0;
	virtual void Visit(WhileStatement* e) = 0;
	virtual void Visit(ForStatement* e) = 0;
	virtual void Visit(FunctionDeclaration* e) = 0;
	virtual void Visit(TopStatements* e) = 0;
	virtual void Visit(EmptyExpression* e) = 0;
};