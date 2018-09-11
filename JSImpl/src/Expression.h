#pragma once

#include "CommonTypes.h"
#include "Lexer.h"

class Expression : public IPLEnableShared<Expression>
{

	public:
		virtual ~Expression() {}
		virtual void Print(std::ostream& os) const {};
};

using ExpressionPtr = IPLSharedPtr<Expression>;

// binary:= expression operator expression
class BinaryExpression : public Expression
{
public:
	virtual ~BinaryExpression() {}
	BinaryExpression(ExpressionPtr exprLeft, ExpressionPtr exprRight, TokenType op);
	virtual void Print(std::ostream& os) const override;
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
private:
	double m_NumValue;
	IPLString m_StringValue;
	bool m_BooleanValue;
	LiteralType m_Type;
};


class IdentifierExpression : public Expression
{
public:
	IdentifierExpression(IPLString& name, ExpressionPtr value);
	virtual ~IdentifierExpression() {}
	virtual void Print(std::ostream& os) const override;
protected:
	IPLString m_Name;
	ExpressionPtr m_Value;
};

class ListExpression : public Expression
{
public:
	ListExpression() {};
	void Push(ExpressionPtr value) { m_Values.push_back(value); };
	virtual ~ListExpression() {}
	virtual void Print(std::ostream& os) const override;
protected:
	IPLVector<ExpressionPtr> m_Values;
};

class VariableDefinitionExpression : public IdentifierExpression
{
public:
	VariableDefinitionExpression(IPLString& name, ExpressionPtr value) : IdentifierExpression(name, value){}
	virtual ~VariableDefinitionExpression() {}
	virtual void Print(std::ostream& os) const override;
};

class BlockStatement : public ListExpression
{
public:
	BlockStatement() {};
	virtual ~BlockStatement() {}
	virtual void Print(std::ostream& os) const override;
private:
};

class LabeledStatement : public Expression
{
public:
	LabeledStatement(IPLString& identifier, ExpressionPtr statements);
	virtual ~LabeledStatement() {}
	virtual void Print(std::ostream& os) const override;
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
private:
	ExpressionPtr m_Initialization;
	ExpressionPtr m_Condition;
	ExpressionPtr m_Iteration;
	ExpressionPtr m_Body;
};

