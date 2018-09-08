#include "Expression.h"
#include <assert.h>

BinaryExpression::BinaryExpression(IPLSharedPtr<Expression> exprLeft, IPLSharedPtr<Expression> exprRight, TokenType op)
: m_Left(exprLeft)
, m_Right(exprRight)
, m_Operator(op)
{}

IPLString BinaryExpression::GetOperatorTypeAsString()
{
	// TODO: Implement
	return "Binary";
}

void BinaryExpression::Print(std::ostream& os)
{
	os << "{ \n";
	os << " Expression Type: Binary" << std::endl;
	os << " Left expr: ";
	m_Left->Print(os);
	
	os << " Right expr:";
	m_Right->Print(os);

	os << " Operator Type: " << GetOperatorTypeAsString();
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
	os << " Expression Type: Unary" << std::endl;
	os << " Expression: ";
	m_Expr->Print(os);

	os << " Operator Type: " << GetOperatorTypeAsString() << std::endl;
	os << " Suffix Type: " << (m_Suffix ? "true" : "false") << std::endl;
	os << "\n}\n";
}

IPLString UnaryExpression::GetOperatorTypeAsString()
{
	switch (m_Operator)
	{
	case TokenType::Delete: return "delete";
	case TokenType::MinusMinus: return "--";
	case TokenType::PlusPlus: return "++";
	case TokenType::Void: return "void";
	case TokenType::Typeof: return "void";
	case TokenType::Plus: return "+";
	case TokenType::Minus: return "-";
	case TokenType::BitwiseNot: return "~";
	case TokenType::Bang: return "!";
	default:
		break;
	}
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

LiteralExpression::LiteralExpression(TokenType type)
{
	if (type == TokenType::Null)
	{
		m_Type = LiteralType::Null;
	}
	else if (type == TokenType::Undefined)
	{
		m_Type = LiteralType::Undefined;
	}
	else
	{
		assert(false);
	}
}

void LiteralExpression::Print(std::ostream& os)
{
	os << "{ \n";
	os << " Expression Type: Unary" << std::endl;
	switch (m_Type)
	{
	case LiteralType::Number:
		os << " Literal Type: Number" << std::endl;
		os << " Literal value: " << m_NumValue << std::endl;
		break;
	case LiteralType::String:
		os << " Literal Type: String" << std::endl;
		os << " Literal value: " << m_StringValue << std::endl;
		break;
	case LiteralType::Boolean:
		os << " Literal Type: Boolean" << std::endl;
		os << " Literal value: " << m_BooleanValue << std::endl;
		break;
	case LiteralType::Null:
		os << " Literal Type: Null" << std::endl;
		os << " Literal value: null" << std::endl;
	case LiteralType::Undefined:
		os << " Literal Type: Undefined" << std::endl;
		os << " Literal value: undefined" << std::endl;
		break;
	default:
		break;
	}

	os << "\n}\n";
}

IdentifierExpression::IdentifierExpression(IPLString name, ExpressionPtr value)
	: m_Name(name)
	, m_Value(value)
{
}

void IdentifierExpression::Print(std::ostream& os)
{
	os << "{ \n";
	os << " Expression Type: Identifier" << std::endl;
	os << "Identifier name: " << m_Name << std::endl;
	m_Value->Print(os);
	os << "\n}\n";
}

void ListExpression::Print(std::ostream& os)
{
	os << "{ \n";
	os << " Expression Type: List" << std::endl;
	for (auto& e : m_Values)
	{
		e->Print(os);
	}
	os << "\n}\n";
}