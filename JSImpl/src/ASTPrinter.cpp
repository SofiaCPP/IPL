#include "ASTPrinter.h"
#include "Expression.h"

ASTPrinter::ASTPrinter(std::ostream& os)
 : os(os)
{}

void Print(std::ostream& os, ASTPrinter &printer, const char* name, const ExpressionPtr& expr)
{
	os << name;
	expr->Accept(printer);
}

void Print(std::ostream& os, ASTPrinter &printer, const char* name, const IPLVector<ExpressionPtr>& list)
{
	os << name << "[ \n";
	for (auto& expr : list)
	{
		expr->Accept(printer);
		os << ",";
	}
	os << name << "] \n";
}

void Print(std::ostream& os, ASTPrinter &printer, const char* name, const IPLVector<IPLString>& list)
{
	os << name << "[ \n";
	for (auto& name : list)
	{
		os << name << ",";
	}
	os << name << "] \n";
}


template<typename T>
void Print(std::ostream& os, ASTPrinter &printer, const char* name, T member)
{
	os << name << " " << member << "\n";
}

IPLString AsString(LiteralType type)
{
	switch (type)
	{
	case LiteralType::Number:
		return "Number";
	case LiteralType::String:
		return "String";
	case LiteralType::Boolean:
		return "Boolean";
	case LiteralType::Null:
		return "Null";
	case LiteralType::Undefined:
		return "Undefined";
	default:
		break;
	}
	return "Undefined";
}

void ASTPrinter::Visit(LiteralExpression* e)
{
	os << "{\n" << "LiteralExpression" << '\n';
	Print(os, *this, "BooleanValue", e->GetBooleanValue());
	Print(os, *this, "LiteralType", AsString(e->GetLiteralType()));
	Print(os, *this, "NumValue", e->GetNumValue());
	Print(os, *this, "StringValue", e->GetStringValue());
	os << "}\n";
}



#define VISIT_MEMBER(type, name, def) Print(os, *this, #name, e->Get##name());

#define GENERATE_AST_PRINTER_FUNCTION(ClassName, Base, MEMBERS_ITERATOR)\
	void ASTPrinter::Visit(ClassName* e)                        \
	{                                                           \
		os << "{\n" << #ClassName << '\n';                      \
		MEMBERS_ITERATOR(VISIT_MEMBER);                         \
		os << "}\n";                                            \
	}


EXPRESSION_DEFINITION_ITERATOR(GENERATE_AST_PRINTER_FUNCTION);