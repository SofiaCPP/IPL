#pragma once
#include "CommonTypes.h"

#include "CommonTypes.h"

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

using ExpressionPtr = IPLSharedPtr<Expression>;
