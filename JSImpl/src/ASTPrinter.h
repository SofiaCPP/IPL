#pragma once

#include "ExpressionVisitor.h"

#include <iosfwd>
class ASTPrinter : public ExpressionVisitor
{
public:
	ASTPrinter(std::ostream& os);
	~ASTPrinter() {}
	virtual void Visit(BinaryExpression* e) override;
	virtual void Visit(UnaryExpression* e) override;
	virtual void Visit(LiteralExpression* e) override;
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
	std::ostream& os;

};
