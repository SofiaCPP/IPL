#include "ASTPrinter.h"

ASTPrinter::ASTPrinter(std::ostream&)
{}
void ASTPrinter::Visit(BinaryExpression*)
{}
void ASTPrinter::Visit(UnaryExpression*)
{}
void ASTPrinter::Visit(LiteralExpression*)
{}
void ASTPrinter::Visit(IdentifierExpression*)
{}
void ASTPrinter::Visit(ListExpression*)
{}
void ASTPrinter::Visit(VariableDefinitionExpression*)
{}
void ASTPrinter::Visit(BlockStatement*)
{}
void ASTPrinter::Visit(LabeledStatement*)
{}
void ASTPrinter::Visit(IfStatement*)
{}
void ASTPrinter::Visit(SwitchStatement*)
{}
void ASTPrinter::Visit(WhileStatement*)
{}
void ASTPrinter::Visit(ForStatement*)
{}
void ASTPrinter::Visit(FunctionDeclaration*)
{}
void ASTPrinter::Visit(TopStatements*)
{}
void ASTPrinter::Visit(EmptyExpression*)
{}
