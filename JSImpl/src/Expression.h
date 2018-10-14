#pragma once

#include "CommonTypes.h"
#include "Lexer.h"
#include "ExpressionVisitor.h"
#include "ExpressionDefinitions.h"

class Expression : public IPLEnableShared<Expression>
{
	public:
		virtual ~Expression() {}
		virtual void Accept(ExpressionVisitor& v) = 0;
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

EXPRESSION_DEFINITION_ITERATOR(GENERATE_EXPRESSION);
#undef GENERATE_EXPRESSION

inline ExpressionPtr CreateEmptyExpression() { return IPLMakeSharePtr<EmptyExpression>(); }
