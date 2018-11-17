#include "ByteCodeGenerator.h"
#include "ExpressionVisitor.h"
#include <algorithm>

class ByteCodeGenerator : public ExpressionVisitor
{
public:
	ByteCodeGenerator() {};
	~ByteCodeGenerator() {};


	virtual void Visit(FunctionDeclaration* e) override;
	virtual void Visit(BlockStatement* e) override;
	virtual void Visit(BinaryExpression* e) override;
	virtual void Visit(LiteralNumber* e) override;
	virtual void Visit(TopStatements* e) override;
	virtual void Visit(ListExpression* e) override;
	virtual void Visit(VariableDefinitionExpression* e) override;
	virtual void Visit(IdentifierExpression* e) override;
	virtual void Visit(EmptyExpression* e) override { (void)e; }
	virtual void Visit(IfStatement* e) override;
	virtual void Visit(ForStatement* e) override;

	IPLString GetCode();
	unsigned ResolveRegisterName(IPLString& name);

private:
	struct Instruction
	{
		enum Type : char
		{
			ADD,
			SUB,
			MUL,
			DIV,
			MOD,
			MOV,
			PRINT,
			READ,
			CALL,
			RET,
			JMP,
			JMPT,
			JMPF,
			DUP,
			PUSH,
			POP,
			SAVE,
			RESTORE,
			LESS,
			EQ,
			LEQ,
			GETG,
			SETG,
			GETUP,
			SETUP,
			GET,
			SET,
			INC,
			DEC,
			AND,
			OR,
			XOR,
			NOT,
			THROW,
			CATCH,
			CONST,
			STRING,
			HALT,
		};

		Type Descriptor;
		IPLString Args[3];
		double Number;
	};
	size_t PushInstruction(Instruction::Type opcode, IPLString& arg1, IPLString& arg2, IPLString& arg3);
	size_t PushInstruction(Instruction::Type opcode, size_t Address);
	size_t PushInstruction(Instruction::Type opcode, IPLString& arg1, size_t Address);

private:
	IPLVector<IPLString> m_RegisterTable;
	IPLVector<Instruction> m_Code;

	IPLStack<IPLString> m_RegisterStack;
	unsigned m_CurrentStackPointer;
	IPLString m_OutputCode;
};

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, IPLString& arg0, IPLString& arg1, IPLString& arg2)
{
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Args[0] = arg0;
	ins.Args[1] = arg1;
	ins.Args[2] = arg2;
	auto address = m_Code.size();
	m_Code.push_back(ins);
	return address;
}

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, size_t Address)
{
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Number = (double)Address;
	auto address = m_Code.size();
	m_Code.push_back(ins);
	return address;
}

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, IPLString& arg0, size_t Address)
{
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Args[0] = arg0;
	ins.Number = (double)Address;
	auto address = m_Code.size();
	m_Code.push_back(ins);
	return address;
}

void ByteCodeGenerator::Visit(FunctionDeclaration* e)
{
	e->GetBody()->Accept(*this);
}

void ByteCodeGenerator::Visit(ListExpression* e)
{
	auto& statements = e->GetValues();
	for (auto& s : statements)
	{
		s->Accept(*this);
	}
}

void ByteCodeGenerator::Visit(BlockStatement* e)
{
	auto& statements = e->GetValues();
	for (auto& s : statements)
	{
		s->Accept(*this);
	}
}

void ByteCodeGenerator::Visit(TopStatements* e)
{
	auto& statements = e->GetValues();

	Instruction current;
	current.Descriptor = Instruction::Type::PUSH;
	auto index = m_Code.size();
	m_Code.push_back(current);
	for (auto& s : statements)
	{
		s->Accept(*this);
	}
	m_Code[index].Number = (unsigned)m_RegisterTable.size();

	current.Descriptor = Instruction::Type::POP;
	current.Number = (unsigned)m_RegisterTable.size();
	m_Code.push_back(current);
}

void ByteCodeGenerator::Visit(VariableDefinitionExpression* e)
{
	auto it = std::find_if(m_RegisterTable.begin(), m_RegisterTable.end(), [&](IPLString& current) {
		return e->GetName() == current;
	});
	if (it != m_RegisterTable.end())
	{
		// TODO: error double definitions
		return;
	}
	m_RegisterTable.push_back(e->GetName());
	if (e->GetValue())
	{
		e->GetValue()->Accept(*this);
	}
	if (!m_RegisterStack.empty())
	{
		Instruction current;
		current.Descriptor = Instruction::Type::MOV;
		current.Args[0] = e->GetName();
		current.Args[1] = m_RegisterStack.top();
		m_RegisterStack.pop();
		m_Code.push_back(current);
	}
}

void ByteCodeGenerator::Visit(BinaryExpression* e)
{
	Instruction current;

	e->GetLeft()->Accept(*this);
	current.Args[1] = m_RegisterStack.top();
	m_RegisterStack.pop();

	e->GetRight()->Accept(*this);
	current.Args[2] = m_RegisterStack.top();
	m_RegisterStack.pop();

	if (e->GetOperator() == TokenType::Equal)
	{
		current.Descriptor = Instruction::Type::MOV;
		current.Args[0] = current.Args[1];
		current.Args[1] = current.Args[2];
		m_Code.push_back(current);
		return;
	}


	IPLString regName = IPLString ("tmp");
	regName += std::to_string(m_RegisterTable.size());
	m_RegisterTable.push_back(regName);
	m_RegisterStack.push(regName);
	current.Args[0] = regName;

	switch (e->GetOperator()) {
	case TokenType::Plus:
		current.Descriptor = Instruction::Type::ADD;
		break;
	case TokenType::Minus:
		current.Descriptor = Instruction::Type::SUB;
		break;
	case TokenType::Star:
		current.Descriptor = Instruction::Type::MUL;
		break;
	case TokenType::Division:
		current.Descriptor = Instruction::Type::DIV;
		break;
	case TokenType::Less:
		current.Descriptor = Instruction::Type::LESS;
		break;
	case TokenType::EqualEqual:
		current.Descriptor = Instruction::Type::EQ;
		break;
	case TokenType::BangEqual:
		NOT_IMPLEMENTED;
		break;
	case TokenType::Equal:

	break;
	default:
		NOT_IMPLEMENTED;
	}
	m_Code.push_back(current);
}

void ByteCodeGenerator::Visit(IfStatement* e)
{
	Instruction current;
	e->GetCondition()->Accept(*this);
	current.Descriptor = Instruction::Type::JMPF;
	current.Args[0] = m_RegisterStack.top();
	current.Number = 0.0;
	m_RegisterStack.pop();
	auto ifIndex = m_Code.size();
	m_Code.push_back(current);

	e->GetIfStatement()->Accept(*this);

	if (e->GetElseStatement())
	{
		current.Descriptor = Instruction::Type::JMP;
		m_Code.push_back(current);
		auto afterTrueBlock = m_Code.size();

		e->GetElseStatement()->Accept(*this);

		// Patching
		m_Code[ifIndex].Number = (double)afterTrueBlock;
		m_Code[afterTrueBlock - 1].Number = (double)m_Code.size();
	}
	else
	{
		m_Code[ifIndex].Number = (double)m_Code.size();
	}
}

void ByteCodeGenerator::Visit(ForStatement* e)
{
	e->GetInitialization()->Accept(*this);
	auto compareAddress = m_Code.size();
	e->GetCondition()->Accept(*this);
	auto endAddress = PushInstruction(Instruction::Type::JMPF, m_RegisterStack.top(), 0);
	m_RegisterStack.pop();
	e->GetBody()->Accept(*this);
	e->GetIteration()->Accept(*this);
	PushInstruction(Instruction::Type::JMP, compareAddress);
	m_Code[endAddress].Number = (double)m_Code.size();
}

void ByteCodeGenerator::Visit(IdentifierExpression* e)
{
	m_RegisterStack.push(e->GetName());
}

void ByteCodeGenerator::Visit(LiteralNumber* e)
{
	IPLString regName = IPLString("const");
	regName += std::to_string(m_RegisterTable.size());
	m_RegisterTable.push_back(regName);
	m_RegisterStack.push(regName);

	PushInstruction(Instruction::Type::CONST, regName, (size_t)e->GetValue());
}

unsigned ByteCodeGenerator::ResolveRegisterName(IPLString& name)
{
	auto it = std::find_if(m_RegisterTable.begin(), m_RegisterTable.end(), [&](IPLString& current) {
		return name == current;
	});
	return unsigned(it - m_RegisterTable.begin());
}

IPLString ByteCodeGenerator::GetCode()
{
	IPLString result;
	auto programCounter = 0;
	for (auto& i : m_Code)
	{
		result += std::to_string(programCounter) + ": ";
		switch (i.Descriptor)
		{
		case ByteCodeGenerator::Instruction::ADD:
			result += "add r"  + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';

			break;
		case ByteCodeGenerator::Instruction::SUB:
			result += "sub r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::MUL:
			result += "mul r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::DIV:
			result += "div r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::MOD:
			result += "mod r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::MOV:
			result += "mov r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::PRINT:
			result += "print r" + std::to_string(ResolveRegisterName(i.Args[0])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::READ:
			result += "read";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::CALL:
			result += "call";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::RET:
			result += "ret";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::JMP:
			result += "jmp " + std::to_string((size_t)i.Number) + '\n';
			break;
		case ByteCodeGenerator::Instruction::JMPT:
			result += "jmpt r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " " + std::to_string((size_t)i.Number) + '\n';
			break;
		case ByteCodeGenerator::Instruction::JMPF:
			result += "jmpf r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " " + std::to_string((size_t)i.Number) + '\n';
			break;
		case ByteCodeGenerator::Instruction::DUP:
			result += "dup";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::PUSH:
			result += "push " + std::to_string((size_t)i.Number) + '\n';
			break;
		case ByteCodeGenerator::Instruction::POP:
			result += "pop " + std::to_string((size_t)i.Number) + '\n';
			break;
		case ByteCodeGenerator::Instruction::SAVE:
			result += "save";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::RESTORE:
			result += "restore";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::LESS:
			result += "less r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::EQ:
			result += "eq r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::LEQ:
			result += "leq r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::GETG:
			result += "getg";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::SETG:
			result += "setg";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::GETUP:
			result += "getup";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::SETUP:
			result += "setup";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::GET:
			result += "get";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::SET:
			result += "set";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::INC:
			result += "inc";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::DEC:
			result += "dec";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::AND:
			result += "and r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::OR:
			result += "or r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::XOR:
			result += "xor r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::NOT:
			result += "not";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::THROW:
			result += "throw";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::CATCH:
			result += "catch";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::CONST:
			result += "const r" + std::to_string(ResolveRegisterName(i.Args[0])) + " " + std::to_string(i.Number) + '\n';
			break;
		case ByteCodeGenerator::Instruction::STRING:
			result += "string";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::HALT:
			result += "halt\n";
			break;
		default:
			NOT_IMPLEMENTED;
			break;
			
		}
		++programCounter;
	}
	result += std::to_string(programCounter) + ": ";
	result += "halt\n";
	return result;
}

IPLString GenerateByteCode(ExpressionPtr program)
{
	ByteCodeGenerator generator;
	program->Accept(generator);

	return generator.GetCode();
}