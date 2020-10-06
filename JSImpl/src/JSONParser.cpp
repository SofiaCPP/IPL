#include "JSONParser.h"
#include "ExpressionDefinitions.h"

#include <unordered_map>
#include <cstdlib>
#include <cstring>

void SkipWhitespaces(const char*& json)
{
	while (*json && (*json == ' ' || *json == '\t' || *json == '\n'))
	{
		++json;
	}
}

inline void PassStringToVal(const char*& json, const char* name)
{
	SkipWhitespaces(json);
	json += 1/*"*/ + strlen(name) + 3/*": */;
}

inline void PassStringToString(const char*& json, const char* name)
{
	PassStringToVal(json, name);
	++json; //"
}

inline void PassStringToArray(const char*& json, const char* name)
{
	PassStringToString(json, name);
}

inline void PassStatementEnd(const char*& json, bool isLast)
{
	if (!isLast)
	{
		++json; //,
	}
}

void JSONParse(const char*& json, const char* name, bool isLast, bool& value);
void JSONParse(const char*& json, const char* name, bool isLast, double& value);
void JSONParse(const char*& json, const char* name, bool isLast, IPLString& value);
void JSONParse(const char*& json, const char* name, bool isLast, TokenType& value);
void JSONParse(const char*& json, const char* name, bool isLast, ExpressionPtr& value);
void JSONParse(const char*& json, const char* name, bool isLast, IPLVector<ExpressionPtr>& value);
void JSONParse(const char*& json, const char* name, bool isLast, IPLVector<IPLString>& value);

#define GENERATE_EXPRESSION_TYPE_ENUMS(ClassName, MEMBERS_ITERATOR)	\
enum class ClassName##Type : unsigned char							\
{																	\
	Invalid															\
};
EXPRESSION_DEFINITION_ITERATOR(GENERATE_EXPRESSION_TYPE_ENUMS);
#undef GENERATE_EXPRESSION_TYPE_ENUM_MEMBERS

template <typename T>
ExpressionPtr JSONParseWrap(const char*& json, const char* eName)
{
	assert(false);
	return nullptr;
}

#define MEMBERS_COUNT(type, name, def)															\
	++membersCount;

#define VISIT_MEMBER(type, name, def)															\
	++currentVisitCount;																		\
	JSONParse(json, #name, currentVisitCount == membersCount, ptr->Get##name##ByRef());			\

#define GENERATE_JSON_PARSE_FUNCTION_DEFINITIONS(ClassName, MEMBERS_ITERATOR)					\
template <>																						\
ExpressionPtr JSONParseWrap<ClassName##Type>(const char*& json, const char* eName)				\
{																								\
	auto ptr = IPLMakeSharePtr<::ClassName>();													\
	int membersCount = 0, currentVisitCount = 0;												\
	(void)currentVisitCount;																	\
	json += strlen("{");																		\
	SkipWhitespaces(json);																		\
	json += strlen("\"ExpresionType\": \"") + strlen(eName) + strlen("\",");					\
	SkipWhitespaces(json);																		\
	MEMBERS_ITERATOR(MEMBERS_COUNT);															\
	SkipWhitespaces(json);																		\
	if (membersCount)																			\
	{																							\
		json += strlen(",");																	\
	}																							\
	SkipWhitespaces(json);																		\
	MEMBERS_ITERATOR(VISIT_MEMBER);																\
	SkipWhitespaces(json);																		\
	json += strlen("}");																		\
	return ptr;																					\
}

EXPRESSION_DEFINITION_ITERATOR(GENERATE_JSON_PARSE_FUNCTION_DEFINITIONS);
#undef GENERATE_JSON_PARSE_FUNCTION_DEFINITIONS
#undef MEMBERS_COUNT
#undef VISIT_MEMBER

IPLString ReadExpressionType(const char* json)
{
	IPLString str;

	SkipWhitespaces(json);
	++json; //{
	SkipWhitespaces(json);
	json += strlen("\"ExpressionType\": \"");

	while (*json != '"')
	{
		str += *json++;
	}

	return str;
}

#define EXPRESSION_PARSE_CALLER_CASES(ClassName, MEMBERS_ITERATOR)	\
	if (type.compare(#ClassName) == 0)								\
		return JSONParseWrap<ClassName##Type>(json, #ClassName);

ExpressionPtr ExpressionParseCaller(const IPLString& type, const char*& json)
{
	EXPRESSION_DEFINITION_ITERATOR(EXPRESSION_PARSE_CALLER_CASES);

	assert(false);
	return nullptr;
}

#undef EXPRESSION_PARSE_CALLER_CASES

std::unordered_map<IPLString, TokenType> GetTokenTable()
{
	std::unordered_map<IPLString, TokenType> table;
	table["LeftParen"] = TokenType::LeftParen;
	table["RightParen"] = TokenType::RightParen;
	table["LeftBrace"] = TokenType::LeftBrace;
	table["RightBrace"] = TokenType::RightBrace;
	table["Comma"] = TokenType::Comma;
	table["Dot"] = TokenType::Dot;
	table["Minus"] = TokenType::Minus;
	table["Plus"] = TokenType::Plus;
	table["Semicolon"] = TokenType::Semicolon;
	table["Star"] = TokenType::Star;
	table["Division"] = TokenType::Division;
	table["Modulo"] = TokenType::Modulo;
	table["BitwiseNot"] = TokenType::BitwiseNot;
	table["BitwiseAnd"] = TokenType::BitwiseAnd;
	table["BitwiseXor"] = TokenType::BitwiseXor;
	table["BitwiseOr"] = TokenType::BitwiseOr;
	table["QuestionMark"] = TokenType::QuestionMark;
	table["Colon"] = TokenType::Colon;
	table["LeftSquareBracket"] = TokenType::LeftSquareBracket;
	table["RightSquareBracket"] = TokenType::RightSquareBracket;
	table["Bang"] = TokenType::Bang;
	table["BangEqual"] = TokenType::BangEqual;
	table["Equal"] = TokenType::Equal;
	table["EqualEqual"] = TokenType::EqualEqual;
	table["StrictEqual"] = TokenType::StrictEqual;
	table["StrictNotEqual"] = TokenType::StrictNotEqual;
	table["Greater"] = TokenType::Greater;
	table["GreaterEqual"] = TokenType::GreaterEqual;
	table["Less"] = TokenType::Less;
	table["LessEqual"] = TokenType::LessEqual;
	table["MinusMinus"] = TokenType::MinusMinus;
	table["PlusPlus"] = TokenType::PlusPlus;
	table["LeftShift"] = TokenType::LeftShift;
	table["RightShift"] = TokenType::RightShift;
	table["LogicalAnd"] = TokenType::LogicalAnd;
	table["LogicalOr"] = TokenType::LogicalOr;
	table["StarEqual"] = TokenType::StarEqual;
	table["DivideEqual"] = TokenType::DivideEqual;
	table["ModuloEqual"] = TokenType::ModuloEqual;
	table["PlusEqual"] = TokenType::PlusEqual;
	table["MinusEqual"] = TokenType::MinusEqual;
	table["LeftShiftEqual"] = TokenType::LeftShiftEqual;
	table["RightShiftEqual"] = TokenType::RightShiftEqual;
	table["BitwiseAndEqual"] = TokenType::BitwiseAndEqual;
	table["BitwiseXorEqual"] = TokenType::BitwiseXorEqual;
	table["BitwiseOrEqual"] = TokenType::BitwiseOrEqual;
	table["Identifier"] = TokenType::Identifier;
	table["String"] = TokenType::String;
	table["Number"] = TokenType::Number;
	table["Break"] = TokenType::Break;
	table["Case"] = TokenType::Case;
	table["Catch"] = TokenType::Catch;
	table["Class"] = TokenType::Class;
	table["Const"] = TokenType::Const;
	table["Continue"] = TokenType::Continue;
	table["Debugger"] = TokenType::Debugger;
	table["Default"] = TokenType::Default;
	table["Delete"] = TokenType::Delete;
	table["Do"] = TokenType::Do;
	table["Else"] = TokenType::Else;
	table["Export"] = TokenType::Export;
	table["Extends"] = TokenType::Extends;
	table["Finally"] = TokenType::Finally;
	table["For"] = TokenType::For;
	table["Function"] = TokenType::Function;
	table["If"] = TokenType::If;
	table["Import"] = TokenType::Import;
	table["In"] = TokenType::In;
	table["Instanceof"] = TokenType::Instanceof;
	table["New"] = TokenType::New;
	table["Return"] = TokenType::Return;
	table["Super"] = TokenType::Super;
	table["Switch"] = TokenType::Switch;
	table["This"] = TokenType::This;
	table["Throw"] = TokenType::Throw;
	table["Try"] = TokenType::Try;
	table["Typeof"] = TokenType::Typeof;
	table["Var"] = TokenType::Var;
	table["Void"] = TokenType::Void;
	table["While"] = TokenType::While;
	table["With"] = TokenType::With;
	table["Yield"] = TokenType::Yield;
	table["Null"] = TokenType::Null;
	table["Undefined"] = TokenType::Undefined;
	table["True"] = TokenType::True;
	table["False"] = TokenType::False;
	table["Eof"] = TokenType::Eof;
	return table;
}

TokenType StringToToken(IPLString str)
{
	static const auto table = GetTokenTable();

	auto it = table.find(str);
	if (it == table.end())
	{
		assert(false);
		return TokenType::Invalid;
	}

	return it->second;
}

void JSONParse(const char*& json, const char* name, bool isLast, bool& value)
{
	PassStringToVal(json, name);

	if (*json == '1')
	{
		value = true;
	}
	else
	{
		value = false;
	}

	++json;

	PassStatementEnd(json, isLast);
}

void JSONParse(const char*& json, const char* name, bool isLast, double& value)
{
	PassStringToVal(json, name);

	value = atof(json);

	while ((*json >= '0' && *json <= '9') || *json == '.')
	{
		++json;
	}

	PassStatementEnd(json, isLast);
}

void JSONParse(const char*& json, const char* name, bool isLast, IPLString& value)
{
	PassStringToString(json, name);

	while (*json != '"')
	{
		value += *json++;
	}

	++json; //"

	PassStatementEnd(json, isLast);
}

void JSONParse(const char*& json, const char* name, bool isLast, TokenType& value)
{
	PassStringToString(json, name);

	IPLString str;
	while (*json != '"')
	{
		str += *json++;
	}

	value = StringToToken(str.c_str());

	++json; //"

	PassStatementEnd(json, isLast);
}

void JSONParse(const char*& json, const char* name, bool isLast, ExpressionPtr& value)
{
	PassStringToVal(json, name);

	const IPLString& type = ReadExpressionType(json);
	value = ExpressionParseCaller(type, json);

	PassStatementEnd(json, isLast);
}

void JSONParse(const char*& json, const char* name, bool isLast, IPLVector<ExpressionPtr>& value)
{
	PassStringToArray(json, name);

	while (true)
	{
		SkipWhitespaces(json);
		if (*json == ']')
		{
			++json;
			PassStatementEnd(json, isLast);
			return;
		}

		const IPLString& type = ReadExpressionType(json);
		value.push_back(ExpressionParseCaller(type, json));

		if (*json == ',')
		{
			++json;
		}
	}
}

void JSONParse(const char*& json, const char* name, bool isLast, IPLVector<IPLString>& value)
{
	PassStringToArray(json, name);

	while (true)
	{
		SkipWhitespaces(json);
		if (*json == ']')
		{
			++json;
			PassStatementEnd(json, isLast);
			return;
		}

		value.push_back(IPLString());

		++json;
		while (*json != '"')
		{
			value.back() += *json;
			++json;
		}
		++json;

		if (*json == ',')
		{
			++json;
		}
	}
}

ExpressionPtr ParseJSON(const char* json)
{
	return JSONParseWrap<TopStatementsType>(json, "TopStatements");
}
