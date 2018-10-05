#include "Expression.h"
#include <assert.h>

//void PrintExpressionMember(ExpressionPtr expr, const char* name, std::ostream& os)
//{
//	os << name << ": {" << std::endl;
//	expr->Print(os);
//	os << "}";
//}
//
//void PrintExpressionMember(IPLVector<ExpressionPtr> exprs, const char* name, std::ostream& os)
//{
//	os << name << ": {" << std::endl;
//	for (auto& it : exprs)
//	{
//		it->Print(os);
//		os << ",\n";
//	}
//	os << "}";
//}
//
//BinaryExpression::BinaryExpression(IPLSharedPtr<Expression> exprLeft, IPLSharedPtr<Expression> exprRight, TokenType op)
//: m_Left(exprLeft)
//, m_Right(exprRight)
//, m_Operator(op)
//{}
//
//IPLString BinaryExpression::GetOperatorTypeAsString() const
//{
//	// TODO: Implement
//	return "Binary";
//}
//
//void BinaryExpression::Print(std::ostream& os) const
//{
//	os << "{ \n";
//	os << " Expression Type: Binary" << std::endl;
//	os << " Left expr: ";
//	m_Left->Print(os);
//	
//	os << " Right expr:";
//	m_Right->Print(os);
//
//	os << " Operator Type: " << GetOperatorTypeAsString();
//	os << "\n}\n";
//}
//
//UnaryExpression::UnaryExpression(IPLSharedPtr<Expression> expr, TokenType op, bool suffix)
//	: m_Expr(expr)
//	, m_Operator(op)
//	, m_Suffix(suffix)
//{
//}
//
//void UnaryExpression::Print(std::ostream& os) const
//{
//	os << "{ \n";
//	os << " Expression Type: Unary" << std::endl;
//	os << " Expression: ";
//	m_Expr->Print(os);
//
//	os << " Operator Type: " << GetOperatorTypeAsString() << std::endl;
//	os << " Suffix Type: " << (m_Suffix ? "true" : "false") << std::endl;
//	os << "\n}\n";
//}
//
//IPLString UnaryExpression::GetOperatorTypeAsString() const
//{
//	switch (m_Operator)
//	{
//	case TokenType::Delete: return "delete";
//	case TokenType::MinusMinus: return "--";
//	case TokenType::PlusPlus: return "++";
//	case TokenType::Void: return "void";
//	case TokenType::Typeof: return "void";
//	case TokenType::Plus: return "+";
//	case TokenType::Minus: return "-";
//	case TokenType::BitwiseNot: return "~";
//	case TokenType::Bang: return "!";
//	default:
//		break;
//	}
//	// TODO: Implement
//	return "Unary";
//}
//
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
//
//void LiteralExpression::Print(std::ostream& os) const
//{
//	os << "{ \n";
//	os << " Expression Type: Unary" << std::endl;
//	switch (m_Type)
//	{
//	case LiteralType::Number:
//		os << " Literal Type: Number" << std::endl;
//		os << " Literal value: " << m_NumValue << std::endl;
//		break;
//	case LiteralType::String:
//		os << " Literal Type: String" << std::endl;
//		os << " Literal value: " << m_StringValue << std::endl;
//		break;
//	case LiteralType::Boolean:
//		os << " Literal Type: Boolean" << std::endl;
//		os << " Literal value: " << m_BooleanValue << std::endl;
//		break;
//	case LiteralType::Null:
//		os << " Literal Type: Null" << std::endl;
//		os << " Literal value: null" << std::endl;
//	case LiteralType::Undefined:
//		os << " Literal Type: Undefined" << std::endl;
//		os << " Literal value: undefined" << std::endl;
//		break;
//	default:
//		break;
//	}
//
//	os << "\n}\n";
//}
//
//IdentifierExpression::IdentifierExpression(IPLString& name)
//	: m_Name(name)
//{
//}
//
//void IdentifierExpression::Print(std::ostream& os) const
//{
//	os << "{ \n";
//	os << " Expression Type: Identifier" << std::endl;
//	os << "Identifier name: " << m_Name << std::endl;
//	os << "\n}\n";
//}
//
//void ListExpression::Print(std::ostream& os) const
//{
//	os << "{ \n";
//	os << " Expression Type: List" << std::endl;
//	for (auto& e : m_Values)
//	{
//		e->Print(os);
//	}
//	os << "\n}\n";
//}
//
//void VariableDefinitionExpression::Print(std::ostream& os) const
//{
//	os << "{ \n";
//	os << " Expression Type: VariableDefinition" << std::endl;
//	os << "Identifier name: " << m_Name << std::endl;
//	m_Value->Print(os);
//	os << "\n}\n";
//}
//
//void BlockStatement::Print(std::ostream& os) const
//{
//	os << "{ \n";
//	os << " Expression Type: BlockStatement" << std::endl;
//	for (auto& e : m_Values)
//	{
//		e->Print(os);
//	}
//	os << "\n}\n";
//}
//
//LabeledStatement::LabeledStatement(IPLString& identifier, ExpressionPtr statement)
//	: m_Identifier(identifier)
//	, m_Statement(statement)
//{
//}
//
//void LabeledStatement::Print(std::ostream& os) const
//{
//	os << "{ LabeledStatement Not implemented yet !!}";
//}
//
//IfStatement::IfStatement(ExpressionPtr cond, ExpressionPtr ifStatement, ExpressionPtr elseStatement)
//	: m_Condition(cond)
//	, m_IfStatement(ifStatement)
//	, m_ElseStatement(elseStatement)
//{
//}
//
//void IfStatement::Print(std::ostream& os) const
//{
//	os << "{ IfStatement  Not implemented yet !!}";
//}
//
//SwitchStatement::SwitchStatement(ExpressionPtr cond, IPLVector<Case>& cases, ExpressionPtr defaultCase)
//	: m_Condition(cond)
//	, m_Cases(cases)
//	, m_DefaultCase(defaultCase)
//{
//}
//
//void SwitchStatement::Print(std::ostream& os) const
//{
//	os << "{ SwitchStatement Not implemented yet !!}";
//}
//
//WhileStatement::WhileStatement(ExpressionPtr cond, ExpressionPtr body, bool doWhile)
//	: m_Condition(cond)
//	, m_Body(body)
//	, m_DoWhile(doWhile)
//{
//}
//
//
//void WhileStatement::Print(std::ostream& os) const
//{
//	os << "{ WhileStatement Not implemented yet !!}";
//}
//
//ForStatement::ForStatement(ExpressionPtr initialization, ExpressionPtr cond, ExpressionPtr iteration, ExpressionPtr body)
//	: m_Initialization(initialization)
//	, m_Condition(cond)
//	, m_Iteration(iteration)
//	, m_Body(body)
//{
//}
//
//void ForStatement::Print(std::ostream& os) const
//{
//	os << "{ Expression Type: ForStatement " << std::endl;
//	os << "{ Initialization:" << std::endl;
//	m_Initialization->Print(os);
//	os << "}" << std::endl;
//	os << "{ Condition:" << std::endl;
//	m_Condition->Print(os);
//	os << "}" << std::endl;
//	os << "{ Iteration:" << std::endl;
//	m_Condition->Print(os);
//	os << "}" << std::endl;
//	os << "{ Body:" << std::endl;
//	m_Body->Print(os);
//	os << "}" << std::endl;
//}
//
//
//FunctionDeclaration::FunctionDeclaration(IPLString& functionName, IPLVector<IPLString>& argumentsIdentifiers, ExpressionPtr body)
//	: m_Name(functionName)
//	, m_ArgumentsIdentifiers(argumentsIdentifiers)
//	, m_Body(body)
//{
//}
//
//void FunctionDeclaration::Print(std::ostream& os) const
//{
//	os << "{ Expression Type: FunctionDeclaration " << std::endl;
//	os << "{ Function name: " << m_Name << std::endl;
//	os << "{ Function parameters: " << std::endl;
//	os << "{ " << std::endl;
//	for (auto it : m_ArgumentsIdentifiers)
//	{
//		os << it;
//	}
//	os << "}" << std::endl;
//	os << "{ Function body:" << std::endl;
//	m_Body->Print(os);
//	os << "}" << std::endl;
//	os << "}" << std::endl;
//}
//
//
//void TopStatements::Print(std::ostream& os) const
//{
//	os << "{ Expression Type: TopStatements " << std::endl;;
//	for (auto it : m_Values)
//	{
//		it->Print(os);
//	}
//	os << "}";
//}
//
//void EmptyExpression::Print(std::ostream& os) const
//{
//	os << "{ EmptyExpression }";
//}
//