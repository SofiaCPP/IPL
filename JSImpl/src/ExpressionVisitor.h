#pragma once

#include "ExpressionsFwd.h"

class ExpressionVisitor
{
public:
	virtual ~ExpressionVisitor() {}

	virtual void Visit(LiteralNull* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(LiteralUndefined* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(LiteralString* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(LiteralNumber* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(LiteralBoolean* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(LiteralField* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(LiteralObject* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(BinaryExpression* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(UnaryExpression* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(IdentifierExpression* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(ListExpression* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(VariableDefinitionExpression* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(BlockStatement* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(LabeledStatement* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(IfStatement* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(SwitchStatement* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(CaseStatement *e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(WhileStatement* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(ForStatement* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(FunctionDeclaration* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(TopStatements* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(EmptyExpression* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(Call* e) { (void)e; NOT_IMPLEMENTED; }
	virtual void Visit(MemberAccess* e) { (void)e; NOT_IMPLEMENTED; }
	
	virtual void Visit(Expression* e) { (void)e; NOT_IMPLEMENTED; } // Unkown case

};