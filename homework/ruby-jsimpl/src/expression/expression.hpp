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

#define CREATE_MEMBER_CONSTRUCTOR_ARGUMENTS(type, name) \
  const type& name,

#define CREATE_MEMBER_CONSTRUCTOR_INITIALIZER(type, name) \
  m_##name(name),

#define CREATE_MEMBER_ATTRIBUTES(type, name) \
  type m_##name;

#define CREATE_EXPRESSION_CLASS(name, MEMBERS_ITERATOR)                                \
  class name : public expression                                                       \
  {                                                                                    \
    bool m_dummy;                                                                      \
  public:                                                                              \
    name(MEMBERS_ITERATOR(CREATE_MEMBER_CONSTRUCTOR_ARGUMENTS) bool __dummy = 0) :     \
      MEMBERS_ITERATOR(CREATE_MEMBER_CONSTRUCTOR_INITIALIZER) m_dummy(__dummy)         \
    { (void)m_dummy; }                                                                 \
                                                                                       \
    MEMBERS_ITERATOR(CREATE_MEMBER_ATTRIBUTES)                                         \
                                                                                       \
    virtual ~name() {}                                                                 \
    virtual void accept(expression_visitor& visitor) override { visitor.visit(this); } \
  };

using expression_ptr = std::shared_ptr<expression>;

CREATE_EXPRESSIONS(CREATE_EXPRESSION_CLASS)

#endif
