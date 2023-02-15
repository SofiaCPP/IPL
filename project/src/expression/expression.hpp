#ifndef EXPRESSION_EXPRESSION_HPP
#define EXPRESSION_EXPRESSION_HPP

#include "expression_visitor.hpp"
#include "expression_type.hpp"

class expression
{
public:
  expression() {}

  virtual ~expression() {}
  virtual void accept(expression_visitor& visitor) = 0;
};

#define CREATE_MEMBER_CONSTRUCTOR_ARGUMENTS(TYPE, NAME, DEFAULT)                       \
  const TYPE& NAME = DEFAULT,

#define CREATE_MEMBER_CONSTRUCTOR_INITIALIZER(TYPE, NAME, DEFAULT)                     \
  m_##NAME(NAME),

#define CREATE_MEMBER_ATTRIBUTES(TYPE, NAME, DEFAULT)                                  \
  TYPE m_##NAME;

#define CREATE_EXPRESSION_CLASS(NAME, MEMBERS_ITERATOR)                                \
  class NAME : public expression                                                       \
  {                                                                                    \
    bool m_dummy;                                                                      \
  public:                                                                              \
    NAME(MEMBERS_ITERATOR(CREATE_MEMBER_CONSTRUCTOR_ARGUMENTS) bool __dummy = 0) :     \
      MEMBERS_ITERATOR(CREATE_MEMBER_CONSTRUCTOR_INITIALIZER) m_dummy(__dummy)         \
    { (void)m_dummy; }                                                                 \
                                                                                       \
    MEMBERS_ITERATOR(CREATE_MEMBER_ATTRIBUTES)                                         \
                                                                                       \
    virtual ~NAME() {}                                                                 \
    virtual void accept(expression_visitor& visitor) override { visitor.visit(this); } \
  };

CREATE_EXPRESSIONS(CREATE_EXPRESSION_CLASS)

#endif
