#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include <list>
#include <map>
#include <string>

namespace SpasmImpl
{
namespace ASM
{
//! Represents symbol table entry for the assembler
class Symbol
{
   public:
    typedef std::list<size_t> Positions_t;
    static const size_t notdefined;
    Symbol();
    Symbol(const std::string&, size_t = 0);

    const std::string& identifier() const;
    size_t definition() const;

    void define(size_t);

    size_t add_position(size_t);

    Positions_t::const_iterator positions_begin() const;
    Positions_t::const_iterator positions_end() const;

   private:
    //! identifier of the symbol
    std::string _identifier;

    //! position of definition of the identifier
    size_t _definition;

    //! list of uses of the identifier (positions)
    Positions_t _positions;
};

//! This is the symbol table of the assembler
class Symbol_Table
{
   private:
    typedef std::map<std::string, Symbol*> Table_t;
    typedef Table_t::iterator iterator;

   public:
    Symbol_Table();
    typedef Table_t::const_iterator const_iterator;

    Symbol* find(const std::string&) const;
    Symbol* insert(const std::string&, size_t);
    Symbol* define(const std::string&, size_t);

    const_iterator begin() const;
    const_iterator end() const;

   private:
    Symbol_Table(const Symbol_Table&);
    Symbol_Table& operator=(const Symbol_Table&);

    Table_t _table;
};
}  // namespace ASM
}  // namespace SpasmImpl

#endif  // SYMBOL_HPP
