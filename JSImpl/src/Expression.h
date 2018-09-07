#pragma once

#include "CommonTypes.h"
#include "Lexer.h"

class Expression : public IPLEnableShared<Expression>
{

	public:
		virtual ~Expression() {}
		virtual void Print(std::ostream& os) {};
};

using ExpressionPtr = IPLSharedPtr<Expression>;

// binary:= expression operator expression
class BinaryExpression : public Expression
{
public:
	virtual ~BinaryExpression() {}
	BinaryExpression(IPLSharedPtr<Expression> exprLeft, IPLSharedPtr<Expression> exprRight, TokenType op);
	virtual void Print(std::ostream& os);
private:
	IPLString GetOperatorTypeAsString();
private:
	IPLSharedPtr<Expression> m_Left;
	IPLSharedPtr<Expression> m_Right;
	TokenType m_Operator;
};

// binary:= expression operator |
//          operator expression
class UnaryExpression : public Expression
{
public:
	virtual ~UnaryExpression() {}
	UnaryExpression(IPLSharedPtr<Expression> expr, TokenType op, bool suffix);
	virtual void Print(std::ostream& os);
private:
	IPLString GetOperatorTypeAsString();
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
	LiteralExpression(LiteralType type);
	virtual void Print(std::ostream& os);
private:
	double m_NumValue;
	IPLString m_StringValue;
	bool m_BooleanValue;
	LiteralType m_Type;
};

