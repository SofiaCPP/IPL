#pragma once

#define NO_MEMBERS(MACRO)

#define LITERAL_BOOLEAN_MEMBERS(MACRO)\
		MACRO(bool, Value, false)

#define LITERAL_NUMBER_MEMBERS(MACRO)\
		MACRO(double, Value, 0.0)

#define LITERAL_STRING_MEMBERS(MACRO)\
		MACRO(IPLString, Value, "")

#define BINARY_EXPRESSION_MEMBERS(MACRO)\
		MACRO(ExpressionPtr, Left, nullptr)\
		MACRO(ExpressionPtr, Right, nullptr)\
		MACRO(TokenType, Operator, TokenType::Invalid)

#define UNARY_EXPRESSION_MEMBERS(MACRO)\
		MACRO(IPLSharedPtr<Expression>, Expr, nullptr)\
		MACRO(TokenType, Operator, TokenType::Invalid)\
		MACRO(bool, Suffix, false)

#define LITERAL_EXPRESSION_MEMBERS(MACRO)\
		MACRO(double, NumValue, 0.)\
		MACRO(IPLString, StringValue, "")\
		MACRO(bool, BooleanValue, false)\
		MACRO(LiteralType, Type, LiteralType::Undefined)

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

#define EXPRESSION_DEFINITION_ITERATOR(MACRO)\
		MACRO(LiteralNull, Expression, NO_MEMBERS)\
		MACRO(LiteralUndefined, Expression, NO_MEMBERS)\
		MACRO(LiteralBoolean, Expression, LITERAL_BOOLEAN_MEMBERS)\
		MACRO(LiteralNumber, Expression, LITERAL_NUMBER_MEMBERS)\
		MACRO(LiteralString, Expression, LITERAL_STRING_MEMBERS)\
		MACRO(BinaryExpression, Expression, BINARY_EXPRESSION_MEMBERS)\
		MACRO(UnaryExpression, Expression, UNARY_EXPRESSION_MEMBERS)\
		MACRO(IdentifierExpression, Expression, IDENTIFIER_EXPRESSION_MEMBERS)\
		MACRO(VariableDefinitionExpression, Expression, VARIABLEDEFINITION_EXPRESSION_MEMBERS)\
		MACRO(ListExpression, Expression, LIST_EXPRESSION_MEMBERS)\
		MACRO(BlockStatement, Expression, BLOCK_EXPRESSION_MEMBERS)\
		MACRO(LabeledStatement, Expression, LABELED_EXPRESSION_MEMBERS)\
		MACRO(IfStatement, Expression, IF_EXPRESSION_MEMBERS)\
		MACRO(SwitchStatement, Expression, SWITCH_EXPRESSION_MEMBERS)\
		MACRO(CaseStatement, Expression, CASE_EXPRESSION_MEMBERS)\
		MACRO(WhileStatement, Expression, WHILE_EXPRESSION_MEMBERS)\
		MACRO(ForStatement, Expression, FOR_EXPRESSION_MEMBERS)\
		MACRO(FunctionDeclaration, Expression, FUNCTION_EXPRESSION_MEMBERS)\
		MACRO(TopStatements, Expression, TOP_EXPRESSION_MEMBERS)\
		MACRO(EmptyExpression, Expression, NO_MEMBERS)
