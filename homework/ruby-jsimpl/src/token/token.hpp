#ifndef TOKEN_TOKEN_HPP
#define TOKEN_TOKEN_HPP

#include "../types.hpp"

#include "token_type.hpp"

class token
{
public:
  token_type m_type;
  string m_content;
};

#endif
