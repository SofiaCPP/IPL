#include "Expression.h"
#include <assert.h>

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
