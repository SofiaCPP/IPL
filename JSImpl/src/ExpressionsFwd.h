#pragma once
#include "CommonTypes.h"

class LiteralNull;
class LiteralUndefined;
class LiteralNumber;
class LiteralString;
class LiteralBoolean;
class LiteralObject;
class Expression;
class BinaryExpression;
class UnaryExpression;
class LiteralExpression;
class IdentifierExpression;
class ListExpression;
class VariableDefinitionExpression;
class BlockStatement;
class LabeledStatement;
class IfStatement;
class SwitchStatement;
class CaseStatement;
class WhileStatement;
class ForStatement;
class FunctionDeclaration;
class TopStatements;
class EmptyExpression;
class CallExpression;

using ExpressionPtr = IPLSharedPtr<Expression>;
