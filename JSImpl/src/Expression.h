#pragma once

#include "CommonTypes.h"
#include "Lexer.h"
#include "ExpressionVisitor.h"

class Expression : public IPLEnableShared<Expression>
{
	public:
		virtual ~Expression() {}
		virtual void Accept(ExpressionVisitor& v) = 0;
};

enum class LiteralType
{
	Number,
	String,
	Boolean,
	Null,
	Undefined
};

class LiteralExpression : public Expression
{
public:

	virtual ~LiteralExpression() {}
	LiteralExpression(double);
	LiteralExpression(IPLString&);
	LiteralExpression(bool);
	LiteralExpression(TokenType type);
	virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }
	double GetNumValue() const { return m_NumValue; }
	const IPLString& GetStringValue() const { return m_StringValue; }
	bool GetBooleanValue() const { return m_BooleanValue; }
	LiteralType GetLiteralType() const { return m_Type; }
private:
	LiteralType m_Type;
	bool m_BooleanValue;
	double m_NumValue;
	IPLString m_StringValue;
};


#define EXPAND_ARGUMENT(type, name, def)\
		type name = def,

#define EXPAND_INITIALIZER_LIST(type, name, def)\
		m_##name(name),

#define GENERATE_GETTERS(type, name, def)\
		const type& Get##name() const { return m_##name; }\
		type& Get##name##ByRef()  { return m_##name; }

#define GENERATE_MEMBER_DEFINITIONS(type, name, def)\
		type m_##name;

#define GENERATE_EXPRESSION(ClassName, Base, MEMBERS_ITERATOR)                 \
	class ClassName : public Base                                              \
	{                                                                          \
		public:                                                                \
		ClassName(MEMBERS_ITERATOR(EXPAND_ARGUMENT) bool __dummy = 0)          \
		:                                                                      \
		MEMBERS_ITERATOR(EXPAND_INITIALIZER_LIST) m_dummy(__dummy) {}          \
		virtual ~ClassName() {}                                                \
		virtual void Accept(ExpressionVisitor& v) override { v.Visit(this); }  \
                                                                               \
		MEMBERS_ITERATOR(GENERATE_GETTERS)                                     \
		private:                                                               \
		MEMBERS_ITERATOR(GENERATE_MEMBER_DEFINITIONS)                          \
		bool m_dummy;                                                          \
	};


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

#define EMPTY_EXPRESSION_MEMBERS(MACRO)

#define EXPRESSION_DEFINITION_ITERATOR(MACRO)\
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
		MACRO(EmptyExpression, Expression, EMPTY_EXPRESSION_MEMBERS)


EXPRESSION_DEFINITION_ITERATOR(GENERATE_EXPRESSION);

inline ExpressionPtr CreateEmptyExpression() { return IPLMakeSharePtr<EmptyExpression>(); }
