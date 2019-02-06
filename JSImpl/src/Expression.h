#pragma once

#include "CommonTypes.h"
#include "Lexer.h"
#include "ExpressionVisitor.h"
#include "ExpressionDefinitions.h"

class Expression : public IPLEnableShared<Expression>
{
	public:
		Expression() : m_Line((unsigned)-1), m_Column((unsigned)-1) {}
		virtual ~Expression() {}
		virtual void Accept(ExpressionVisitor& v) = 0;
		unsigned GetLine() { return m_Line; };
		unsigned GetColumn() { return m_Column; };
		void SetLocation(unsigned l, unsigned c) { m_Line = l; m_Column = c; };
	private:
		unsigned m_Line = 0;
		unsigned m_Column = 0;
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

#define GENERATE_EXPRESSION(ClassName, MEMBERS_ITERATOR)                       \
	class ClassName : public Expression                                        \
	{                                                                          \
		public:                                                                \
		ClassName(MEMBERS_ITERATOR(EXPAND_ARGUMENT) bool __dummy = 0)          \
		:                                                                      \
		MEMBERS_ITERATOR(EXPAND_INITIALIZER_LIST) m_dummy(__dummy) {}          \
		virtual ~ClassName() {}                                                \
		virtual void Accept(ExpressionVisitor& v) override { if (!v.IsInSkipMode()) v.Visit(this); }  \
                                                                               \
		MEMBERS_ITERATOR(GENERATE_GETTERS)                                     \
		private:                                                               \
		MEMBERS_ITERATOR(GENERATE_MEMBER_DEFINITIONS)                          \
		bool m_dummy;                                                          \
	};

EXPRESSION_DEFINITION_ITERATOR(GENERATE_EXPRESSION);
#undef GENERATE_EXPRESSION

inline ExpressionPtr CreateEmptyExpression() { return IPLMakeSharePtr<EmptyExpression>(); }
