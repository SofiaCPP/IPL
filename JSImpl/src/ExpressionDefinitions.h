#pragma once

#define NO_MEMBERS(MACRO)

#define LITERAL_BOOLEAN_MEMBERS(MACRO)\
		MACRO(bool, Value, false)

#define LITERAL_NUMBER_MEMBERS(MACRO)\
		MACRO(double, Value, 0.0)

#define LITERAL_STRING_MEMBERS(MACRO)\
		MACRO(IPLString, Value, "")

#define LITERAL_FIELD_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Identifier, nullptr)\
		MACRO(ExpressionPtr, Value, nullptr)

#define LITERAL_OBJECT_MEMBERS(MACRO)\
		MACRO(IPLVector<ExpressionPtr>, Values, IPLVector<ExpressionPtr>())

#define BINARY_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Left, nullptr)\
		MACRO(ExpressionPtr, Right, nullptr)\
		MACRO(TokenType, Operator, TokenType::Invalid)

#define UNARY_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLSharedPtr<Expression>, Expr, nullptr)\
		MACRO(TokenType, Operator, TokenType::Invalid)\
		MACRO(bool, Suffix, false)

#define IDENTIFIER_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLString, Name, "")

#define VARIABLEDEFINITION_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLString, Name, "")\
		MACRO(ExpressionPtr, Value, nullptr)

#define LIST_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLVector<ExpressionPtr>, Values, IPLVector<ExpressionPtr>())

#define BLOCK_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLVector<ExpressionPtr>, Values, IPLVector<ExpressionPtr>())

#define LABELED_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLString, Identifier, "")\
		MACRO(ExpressionPtr, Statement, nullptr)

#define IF_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Condition, nullptr)\
		MACRO(ExpressionPtr, IfStatement, nullptr)\
		MACRO(ExpressionPtr, ElseStatement, nullptr)

#define CASE_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Condition, nullptr)\
		MACRO(ExpressionPtr, Body, nullptr)

#define SWITCH_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Condition, nullptr)\
		MACRO(IPLVector<ExpressionPtr>, Cases, IPLVector<ExpressionPtr>())\
		MACRO(ExpressionPtr, DefaultCase, nullptr)

#define WHILE_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Condition, nullptr)\
		MACRO(ExpressionPtr, Body, nullptr)\
		MACRO(bool, DoWhile, false)

#define FOR_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Initialization, nullptr)\
		MACRO(ExpressionPtr, Condition, nullptr)\
		MACRO(ExpressionPtr, Iteration, nullptr)\
		MACRO(ExpressionPtr, Body, nullptr)

#define FUNCTION_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLString, Name, "")\
		MACRO(IPLVector<IPLString>, ArgumentsIdentifiers, IPLVector<IPLString>())\
		MACRO(ExpressionPtr, Body, nullptr)

#define TOP_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLVector<ExpressionPtr>, Values, IPLVector<ExpressionPtr>())

#define CALL_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, ObjectOrCall, ExpressionPtr())\
		MACRO(ExpressionPtr, Member, ExpressionPtr())

#define MEMBER_ACCESS_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLString, Name, "")

#define EXPRESSION_DEFINITION_ITERATOR(MACRO)\
		MACRO(LiteralNull, NO_MEMBERS)\
		MACRO(LiteralUndefined, NO_MEMBERS)\
		MACRO(LiteralBoolean, LITERAL_BOOLEAN_MEMBERS)\
		MACRO(LiteralNumber, LITERAL_NUMBER_MEMBERS)\
		MACRO(LiteralString, LITERAL_STRING_MEMBERS)\
		MACRO(LiteralField, LITERAL_FIELD_MEMBERS)\
		MACRO(LiteralObject, LITERAL_OBJECT_MEMBERS)\
		MACRO(BinaryExpression, BINARY_EXPRESSION_MEMBERS)\
		MACRO(UnaryExpression, UNARY_EXPRESSION_MEMBERS)\
		MACRO(IdentifierExpression, IDENTIFIER_EXPRESSION_MEMBERS)\
		MACRO(VariableDefinitionExpression, VARIABLEDEFINITION_EXPRESSION_MEMBERS)\
		MACRO(ListExpression, LIST_EXPRESSION_MEMBERS)\
		MACRO(BlockStatement, BLOCK_EXPRESSION_MEMBERS)\
		MACRO(LabeledStatement, LABELED_EXPRESSION_MEMBERS)\
		MACRO(IfStatement, IF_EXPRESSION_MEMBERS)\
		MACRO(SwitchStatement, SWITCH_EXPRESSION_MEMBERS)\
		MACRO(CaseStatement, CASE_EXPRESSION_MEMBERS)\
		MACRO(WhileStatement, WHILE_EXPRESSION_MEMBERS)\
		MACRO(ForStatement, FOR_EXPRESSION_MEMBERS)\
		MACRO(FunctionDeclaration, FUNCTION_EXPRESSION_MEMBERS)\
		MACRO(TopStatements, TOP_EXPRESSION_MEMBERS)\
		MACRO(EmptyExpression, NO_MEMBERS)\
		MACRO(Call, CALL_EXPRESSION_MEMBERS)\
		MACRO(MemberAccess, MEMBER_ACCESS_EXPRESSION_MEMBERS)
