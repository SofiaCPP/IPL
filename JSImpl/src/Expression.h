#pragma once

#include "CommonTypes.h"
#include "Lexer.h"

class Expression;
class BinaryExpression;
class UnaryExpression;
class LiteralExpression;
class IdentifierExpression;
class ListExpression;
class VariableDefinitionExpression;
class BlockStatement;
class LabeledStatement;
class IfStatement;
class SwitchStatement;
class WhileStatement;
class ForStatement;
class FunctionDeclaration;
class TopStatements;
class EmptyExpression;
using ExpressionPtr = IPLSharedPtr<Expression>;

class ExpressionVisitor
{
public:
	~ExpressionVisitor(){}
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
	virtual void Visit(WhileStatement* e) = 0;
	virtual void Visit(ForStatement* e) = 0;
	virtual void Visit(FunctionDeclaration* e) = 0;
	virtual void Visit(TopStatements* e) = 0;
	virtual void Visit(EmptyExpression* e) = 0;
};

class Expression : public IPLEnableShared<Expression>
{

	public:
		virtual ~Expression() {}
		virtual void Print(std::ostream& os) const {};
		virtual void Accept(ExpressionVisitor& v) = 0;
};


// binary:= expression operator expression
class BinaryExpression : public Expression
{
public:
	virtual ~BinaryExpression() {}
	BinaryExpression(ExpressionPtr exprLeft, ExpressionPtr exprRight, TokenType op);
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	IPLString GetOperatorTypeAsString() const;
private:
	ExpressionPtr m_Left;
	ExpressionPtr m_Right;
	TokenType m_Operator;
};

// binary:= expression operator |
//          operator expression
class UnaryExpression : public Expression
{
public:
	virtual ~UnaryExpression() {}
	UnaryExpression(IPLSharedPtr<Expression> expr, TokenType op, bool suffix);
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	IPLString GetOperatorTypeAsString() const;
private:
	IPLSharedPtr<Expression> m_Expr;
	TokenType m_Operator;
	bool m_Suffix;
};

// literal:= number | string | true | false | null | undefined
class LiteralExpression : public Expression
{
public:
	enum LiteralType
	{
		Number,
		String,
		Boolean,
		Null,
		Undefined,
	};
	virtual ~LiteralExpression() {}
	LiteralExpression(double);
	LiteralExpression(IPLString&);
	LiteralExpression(bool);
	LiteralExpression(TokenType type);
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	double m_NumValue;
	IPLString m_StringValue;
	bool m_BooleanValue;
	LiteralType m_Type;
};


class IdentifierExpression : public Expression
{
public:
	IdentifierExpression(IPLString& name);
	virtual ~IdentifierExpression() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
protected:
	IPLString m_Name;

};

class ListExpression : public Expression
{
public:
	ListExpression() {};
	void Push(ExpressionPtr value) { m_Values.push_back(value); };
	virtual ~ListExpression() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
protected:
	IPLVector<ExpressionPtr> m_Values;
};

class VariableDefinitionExpression : public IdentifierExpression
{
public:
	VariableDefinitionExpression(IPLString& name, ExpressionPtr value) : IdentifierExpression(name), m_Value(value){}
	virtual ~VariableDefinitionExpression() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
protected:
	ExpressionPtr m_Value;
};

class BlockStatement : public ListExpression
{
public:
	BlockStatement() {};
	virtual ~BlockStatement() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
};

class LabeledStatement : public Expression
{
public:
	LabeledStatement(IPLString& identifier, ExpressionPtr statements);
	virtual ~LabeledStatement() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	IPLString m_Identifier;
	ExpressionPtr m_Statement;
};

class IfStatement : public Expression
{
public:
	IfStatement(ExpressionPtr cond, ExpressionPtr ifStatement, ExpressionPtr elseStatement);
	virtual ~IfStatement() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	ExpressionPtr m_Condition;
	ExpressionPtr m_IfStatement;
	ExpressionPtr m_ElseStatement;
};

class SwitchStatement : public Expression
{
public:
	struct Case
	{
		ExpressionPtr Condition;
		ExpressionPtr Body;
	};
	SwitchStatement(ExpressionPtr cond, IPLVector<Case>& cases, ExpressionPtr defaultCase);
	virtual ~SwitchStatement() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	ExpressionPtr m_Condition;
	IPLVector<Case> m_Cases;
	ExpressionPtr m_DefaultCase;
};

class WhileStatement : public Expression
{
public:
	WhileStatement(ExpressionPtr cond, ExpressionPtr body, bool doWhile);
	virtual ~WhileStatement() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	ExpressionPtr m_Condition;
	ExpressionPtr m_Body;
	bool m_DoWhile;
};

class ForStatement : public Expression
{
public:
	ForStatement(ExpressionPtr initialization, ExpressionPtr cond, ExpressionPtr iteration, ExpressionPtr body);
	virtual ~ForStatement() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	ExpressionPtr m_Initialization;
	ExpressionPtr m_Condition;
	ExpressionPtr m_Iteration;
	ExpressionPtr m_Body;
};

class FunctionDeclaration : public Expression
{
public:
	FunctionDeclaration(IPLString& functionName, IPLVector<IPLString>& argumentsIdentifiers, ExpressionPtr body);
	virtual ~FunctionDeclaration() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
private:
	IPLString m_Name;
	IPLVector<IPLString> m_ArgumentsIdentifiers;
	ExpressionPtr m_Body;
};

class TopStatements : public ListExpression
{
public:
	TopStatements() {}
	virtual ~TopStatements() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
};

class EmptyExpression : public Expression
{
public:
	EmptyExpression() {}
	virtual ~EmptyExpression() {}
	virtual void Print(std::ostream& os) const override;
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
};

inline ExpressionPtr CreateEmptyExpression() { return IPLMakeSharePtr<EmptyExpression>(); }
