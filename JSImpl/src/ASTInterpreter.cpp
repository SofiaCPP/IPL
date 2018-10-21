#include "ASTInterpreter.h"
#include "Expression.h"
#include <cassert>
#include <iostream>

ASTInterpreter::ASTInterpreter()
{}

ASTInterpreter::~ASTInterpreter()
{}

ASTInterpreter::ValueStack ASTInterpreter::Run(Expression* program)
{
    program->Accept(*this);
    ValueStack result;
    m_Evaluation.swap(result);
    return result;
}

void ASTInterpreter::RunExpression(const ExpressionPtr& e)
{
    if (e) {
        e->Accept(*this);
    }
}

bool ASTInterpreter::EvalToBool(const ExpressionPtr& e)
{
    e->Accept(*this);
    auto result = m_Evaluation.top();
    m_Evaluation.pop();
    return result != 0.0;
}

#define NOT_IMPLEMENTED (void)e; assert(0 && "not-implemented")

void ASTInterpreter::Visit(LiteralNull* e) {
   NOT_IMPLEMENTED; 
}

void ASTInterpreter::Visit(LiteralUndefined* e) {
   NOT_IMPLEMENTED; 
}
void ASTInterpreter::Visit(LiteralString* e) {
   NOT_IMPLEMENTED; 
}

void ASTInterpreter::Visit(LiteralNumber* e) {
    m_Evaluation.push(e->GetValue());
}
void ASTInterpreter::Visit(LiteralBoolean* e) {
    m_Evaluation.push(e->GetValue());
}

void ASTInterpreter::Visit(BinaryExpression* e) {
    e->GetLeft()->Accept(*this);
    e->GetRight()->Accept(*this);
    auto right = m_Evaluation.top();
    m_Evaluation.pop();

    auto left = m_Evaluation.top();
    m_Evaluation.pop();

    switch (e->GetOperator()) {
        case TokenType::Plus:
            std::cout << "PLUS:" << left << ':' << right << std::endl;
            m_Evaluation.push(left + right);
            break;
        case TokenType::Minus:
            m_Evaluation.push(left - right);
            break;
        case TokenType::Star:
            m_Evaluation.push(left * right);
            break;
        case TokenType::Division:
            m_Evaluation.push(left / right);
            break;
        default:
            NOT_IMPLEMENTED;
    }
}

void ASTInterpreter::Visit(UnaryExpression* e) {
    NOT_IMPLEMENTED;
}

void ASTInterpreter::Visit(IdentifierExpression* e) {
    auto values = m_Variables.find(e->GetName());
    if (values != m_Variables.end()) {
        m_Evaluation.push(values->second.top());
    } else {
        throw "Not-defined";
    }
}
void ASTInterpreter::Visit(ListExpression* e) {
    for (const auto& stmt: e->GetValues()) {
        stmt->Accept(*this);
    }
}

void ASTInterpreter::Visit(VariableDefinitionExpression* e) {
    e->GetValue()->Accept(*this);
    m_Variables[e->GetName()].push(m_Evaluation.top());
    m_Evaluation.pop();
}

void ASTInterpreter::Visit(BlockStatement* e) {
    for (const auto& stmt: e->GetValues()) {
        stmt->Accept(*this);
    }
}

void ASTInterpreter::Visit(LabeledStatement* e) {
    RunExpression(e->GetStatement());
}

void ASTInterpreter::Visit(IfStatement* e) {
    if (EvalToBool(e->GetCondition())) {
        RunExpression(e->GetIfStatement());
    } else {
        RunExpression(e->GetElseStatement());
    }
}

void ASTInterpreter::Visit(SwitchStatement* e) { NOT_IMPLEMENTED;}
void ASTInterpreter::Visit(CaseStatement *e) { NOT_IMPLEMENTED;}

void ASTInterpreter::Visit(WhileStatement* e) {
    if (e->GetDoWhile()) {
        RunExpression(e->GetBody());
    }
    while (EvalToBool(e->GetCondition())) {
        RunExpression(e->GetBody());
    }
}

void ASTInterpreter::Visit(ForStatement* e) {
    for (RunExpression(e->GetInitialization());
            EvalToBool(e->GetCondition());
            RunExpression(e->GetIteration())) {
        RunExpression(e->GetBody());
    }
}

void ASTInterpreter::Visit(FunctionDeclaration* e) { NOT_IMPLEMENTED;}
void ASTInterpreter::Visit(TopStatements* e) {
    for (const auto& stmt: e->GetValues()) {
        stmt->Accept(*this);
    }
}

void ASTInterpreter::Visit(EmptyExpression* e) {
    NOT_IMPLEMENTED;
}
