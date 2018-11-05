#include <list>
#include <string>

#include "symbol.hpp"

namespace SpasmImpl
{
namespace ASM
{
const size_t Symbol::notdefined = ~0ull;

Symbol::Symbol() : _definition(notdefined) {}

Symbol::Symbol(const std::string& identifier, size_t definition)
    : _identifier(identifier), _definition(definition)
{
}

const std::string& Symbol::identifier() const
{
    return _identifier;
}

size_t Symbol::definition() const
{
    return _definition;
}

void Symbol::define(size_t definition)
{
    _definition = definition;
}

size_t Symbol::add_position(size_t position)
{
    _positions.push_back(position);

    return _definition;
}

Symbol::Positions_t::const_iterator Symbol::positions_begin() const
{
    return _positions.begin();
}

Symbol::Positions_t::const_iterator Symbol::positions_end() const
{
    return _positions.end();
}

Symbol_Table::Symbol_Table() {}

/*!
** Search the table for symbol by identifier
**
** \param identifier - identifier of the symbol
**
** \return pointer to the symbol if it exists or NULL otherwise.
*/
Symbol* Symbol_Table::find(const std::string& identifier) const
{
    const_iterator i = _table.find(identifier);
    return (i != _table.end()) ? i->second : NULL;
}

/*!
** Insert symbol with identifier indentifier on position position
** of just add new position for symbol.
** \param identifier - identifer of the symbol
** \param position - position of the symbol
**
** \return pointer to the symbol
*/
Symbol* Symbol_Table::insert(const std::string& identifier, size_t position)
{
    Symbol* psymbol = find(identifier);
    if (psymbol == NULL)
    {
        psymbol = new Symbol(identifier);
        _table.insert(std::make_pair(identifier, psymbol));
    }
    psymbol->add_position(position);

    return psymbol;
}

/*!
** define symbol with identifier indentifier
** If the symbol already exists its definition is changed
** \param identifier - identifer of the symbol
** \param definition - definition of the symbol
**
** \return pointer to the symbol
*/
Symbol* Symbol_Table::define(const std::string& identifier, size_t definition)
{
    Symbol* psymbol = find(identifier);
    if (psymbol == NULL)
    {
        psymbol = new Symbol(identifier, definition);
        _table.insert(std::make_pair(identifier, psymbol));
    }
    else
        psymbol->define(definition);

    return psymbol;
}

Symbol_Table::const_iterator Symbol_Table::begin() const
{
    return _table.begin();
}

Symbol_Table::const_iterator Symbol_Table::end() const
{
    return _table.end();
}

}  // namespace ASM
}  // namespace SpasmImpl
