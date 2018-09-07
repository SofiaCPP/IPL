#include "Expression.h"

BinaryExpression::BinaryExpression(IPLSharedPtr<Expression> exprLeft, IPLSharedPtr<Expression> exprRight, TokenType op)
: m_Left(exprLeft)
, m_Right(exprRight)
, m_Operator(op)
{}

IPLString BinaryExpression::GetBinaryTypeAsString()
{
	// TODO: Implement
	return "Binary";
}

void BinaryExpression::Print(std::ostream& os)
{
	os << "{ \n";
	os << " Expression Type: Binary";
	os << " Left expr: ";
	m_Left->Print(os);
	
	os << " Right expr:";
	m_Right->Print(os);

	os << " Operator Type: " << GetBinaryTypeAsString();
	os << "\n}\n";
}



UnaryExpression::UnaryExpression(IPLSharedPtr<Expression> expr, TokenType op, bool suffix)
	: m_Expr(expr)
	, m_Operator(op)
	, m_Suffix(suffix)
{
}

void UnaryExpression::Print(std::ostream& os)
{
	os << "{ \n";
	os << " Expression Type: Unary";
	os << " Expr: ";
	m_Expr->Print(os);

	os << " Operator Type: " << GetBinaryTypeAsString() << std::endl;
	os << "\n}\n";
}

IPLString UnaryExpression::GetBinaryTypeAsString()
{
	// TODO: Implement
	return "Unary";
}

LiteralExpression::LiteralExpression(double v)
	:m_Type(LiteralType::Number)
	, m_NumValue(v)
{}

LiteralExpression::LiteralExpression(IPLString& v)
	: m_Type(LiteralType::String)
	, m_StringValue(v)
{}

LiteralExpression::LiteralExpression(bool v)
	: m_Type(LiteralType::Boolean)
	, m_BooleanValue(v)
{}

LiteralExpression::LiteralExpression(LiteralType type)
	: m_Type(type)
{}

void LiteralExpression::Print(std::ostream& os)
{
	os << "{ \n";
	os << " Literal Type: Unary";
	os << " Literal value: ";
	switch (m_Type)
	{
	case LiteralType::Number:
		os << m_NumValue;
		break;
	case LiteralType::String:
		os << m_StringValue;
		break;
	case LiteralType::Boolean:
		os << m_BooleanValue;
		break;
	case LiteralType::Null:
		os << "null";
	case LiteralType::Undefined:
		os << "undefined";
		break;
	default:
		break;
	}

	os << "\n}\n";
}
