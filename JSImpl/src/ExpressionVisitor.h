#pragma once

#include "ExpressionsFwd.h"

class ExpressionVisitor
{
public:
	~ExpressionVisitor() {}
	virtual void Visit(LiteralNull* e) = 0;
	virtual void Visit(LiteralUndefined* e) = 0;
	virtual void Visit(LiteralString* e) = 0;
	virtual void Visit(LiteralNumber* e) = 0;
	virtual void Visit(LiteralBoolean* e) = 0;
	virtual void Visit(LiteralObject* e) = 0;
	virtual void Visit(BinaryExpression* e) = 0;
	virtual void Visit(UnaryExpression* e) = 0;
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
	virtual void Visit(CallExpression* e) = 0;
};