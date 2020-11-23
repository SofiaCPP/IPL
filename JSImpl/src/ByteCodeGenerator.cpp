#include "ByteCodeGenerator.h"
#include "ExpressionVisitor.h"
#include <algorithm>
#include <sstream>
#include <iterator>

class ByteCodeGenerator : public ExpressionVisitor
{
public:

	ByteCodeGenerator(const ByteCodeGeneratorOptions& o, const IPLVector<IPLString>& source) : m_Source(source), m_Options(o) {};
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
	virtual void Visit(WhileStatement* e) override;
	virtual void Visit(UnaryExpression* e) override;
	virtual void Visit(LiteralField* e) override;
	virtual void Visit(LiteralObject* e) override;
	virtual void Visit(LiteralString* e) override;
	virtual void Visit(MemberAccess* e) override;
	virtual void Visit(Call* e) override;

	IPLString GetCode();
	unsigned ResolveRegisterName(IPLString& name);

private:
	void AddDebugInformation(Expression* e);
	struct Instruction
	{
		enum Type : char
		{
			FIRST = 0,
			ADD = FIRST,
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
			LESSEQ,
			GREATER,
			GREATEREQ,
			EQ,
			NEQ,
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
			DEBUG,
			LAST = DEBUG
		};

		Type Descriptor;
		IPLString Args[3];
		union
		{
			double Double[3];
			long long Int[3];
			unsigned long long Address[3];
		} Values;
	};
	size_t PushInstruction(Instruction::Type opcode, const IPLString& arg1 = "", const IPLString& arg2 = "", const IPLString& arg3 = "");
	size_t PushInstruction(Instruction::Type opcode, size_t Address);
	size_t PushInstruction(Instruction::Type opcode, const IPLString& arg1, size_t Address);
	size_t PushInstruction(Instruction::Type opcode, int Int);
	size_t PushInstruction(Instruction::Type opcode, const IPLString& arg0, double value);

	void PushConst(double c);
	IPLString CreateRegister();
	bool CheckOpCode(int opcode) { return opcode >= Instruction::Type::FIRST && opcode <= Instruction::Type::LAST; }
private:
	IPLVector<IPLString> m_RegisterTable;
	IPLVector<Instruction> m_Code;
	IPLVector<IPLString> m_Source;

	IPLStack<IPLString> m_RegisterStack;
	IPLString m_OutputCode;
	ByteCodeGeneratorOptions m_Options;
};

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, const IPLString& arg0, const IPLString& arg1, const IPLString& arg2)
{
	assert(CheckOpCode(opcode));
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Args[0] = arg0;
	ins.Args[1] = arg1;
	ins.Args[2] = arg2;
	m_Code.push_back(ins);
	return m_Code.size() - 1;;
}

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, size_t Address)
{
	assert(CheckOpCode(opcode));
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Values.Address[0] = Address;
	m_Code.push_back(ins);
	return m_Code.size() - 1;;
}

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, int Int)
{
	assert(CheckOpCode(opcode));
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Values.Int[0] = Int;
	m_Code.push_back(ins);
	return m_Code.size() - 1;;
}

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, const IPLString& arg0, size_t Address)
{
	assert(CheckOpCode(opcode));
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Args[0] = arg0;
	ins.Values.Address[0] = Address;
	m_Code.push_back(ins);
	return m_Code.size() - 1;;
}

size_t ByteCodeGenerator::PushInstruction(Instruction::Type opcode, const IPLString& arg0, double value)
{
	assert(CheckOpCode(opcode));
	Instruction ins;
	ins.Descriptor = opcode;
	ins.Args[0] = arg0;
	ins.Values.Double[0] = value;
	m_Code.push_back(ins);
	return m_Code.size() - 1;;
}

void ByteCodeGenerator::PushConst(double c)
{
	IPLString regName = CreateRegister();
	m_RegisterStack.push(regName);

	PushInstruction(Instruction::Type::CONST, regName, c);
}

IPLString ByteCodeGenerator::CreateRegister()
{
	IPLString regName = IPLString("tmp");
	regName += std::to_string(m_RegisterTable.size());
	m_RegisterTable.push_back(regName);
	return regName;
}

void ByteCodeGenerator::AddDebugInformation(Expression* e)
{
	if (!m_Options.AddDebugInformation || e->GetLine() == (unsigned)-1 || e->GetColumn() == (unsigned)-1)
	{
		return;
	}
	Instruction ins;
	ins.Descriptor = Instruction::Type::DEBUG;
	ins.Values.Int[0] = e->GetLine();
	ins.Values.Int[1] = e->GetColumn();
	m_Code.push_back(ins);
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

	auto startAddress = PushInstruction(Instruction::Type::PUSH, (int)0);
	for (auto& s : statements)
	{
		s->Accept(*this);
	}
	m_Code[startAddress].Values.Int[0] = (int)m_RegisterTable.size();

	PushInstruction(Instruction::Type::POP, (int)m_RegisterTable.size());
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
	AddDebugInformation(e);
	if (!m_RegisterStack.empty())
	{
		PushInstruction(Instruction::Type::MOV, e->GetName(), m_RegisterStack.top(), "");
		m_RegisterStack.pop();
	}
}

void ByteCodeGenerator::Visit(BinaryExpression* e)
{
	e->GetRight()->Accept(*this);
	auto r = m_RegisterStack.top();
	e->GetLeft()->Accept(*this);
	auto l = m_RegisterStack.top();

	AddDebugInformation(e);
	if (e->GetOperator() == TokenType::Equal)
	{
		PushInstruction(Instruction::Type::MOV, l, r);
		return;
	}

	IPLString o = CreateRegister();
	m_RegisterStack.push(o);

	switch (e->GetOperator()) {
	case TokenType::Plus:
		PushInstruction(Instruction::Type::ADD, o, l, r);
		return;
	case TokenType::Minus:
		PushInstruction(Instruction::Type::SUB, o, l, r);
		return;
	case TokenType::Star:
		PushInstruction(Instruction::Type::MUL, o, l, r);
		return;
	case TokenType::Division:
		PushInstruction(Instruction::Type::DIV, o, l, r);
		return;
	case TokenType::Less:
		PushInstruction(Instruction::Type::LESS, o, l, r);
		return;
	case TokenType::LessEqual:
		PushInstruction(Instruction::Type::LESSEQ, o, l, r);
		return;
	case TokenType::Greater:
		PushInstruction(Instruction::Type::GREATER, o, l, r);
		return;
	case TokenType::GreaterEqual:
		PushInstruction(Instruction::Type::GREATEREQ, o, l, r);
		break;
	case TokenType::EqualEqual:
		PushInstruction(Instruction::Type::EQ, o, l, r);
		return;
	case TokenType::BangEqual:
		PushInstruction(Instruction::Type::NEQ,o,l,r);
		break;
	case TokenType::Equal:

	break;
	default:
		NOT_IMPLEMENTED;
	}
}

void ByteCodeGenerator::Visit(IfStatement* e)
{
	e->GetCondition()->Accept(*this);
	auto ifAddress = PushInstruction(Instruction::Type::JMPF, m_RegisterStack.top(), size_t(0));

	e->GetIfStatement()->Accept(*this);

	if (e->GetElseStatement())
	{
		auto blockEndAddress = PushInstruction(Instruction::Type::JMP);

		e->GetElseStatement()->Accept(*this);

		// Patching
		m_Code[ifAddress].Values.Address[0] = blockEndAddress + 1;
		m_Code[blockEndAddress].Values.Address[0] = m_Code.size();
	}
	else
	{
		m_Code[ifAddress].Values.Address[0] = m_Code.size();
	}
}

void ByteCodeGenerator::Visit(ForStatement* e)
{
	e->GetInitialization()->Accept(*this);
	auto compareAddress = m_Code.size();
	e->GetCondition()->Accept(*this);
	auto endAddress = PushInstruction(Instruction::Type::JMPF, m_RegisterStack.top(), (size_t)0);
	m_RegisterStack.pop();
	e->GetBody()->Accept(*this);
	e->GetIteration()->Accept(*this);
	PushInstruction(Instruction::Type::JMP, compareAddress);
	m_Code[endAddress].Values.Address[0] = m_Code.size();
}

void ByteCodeGenerator::Visit(WhileStatement* e)
{
	auto compareAddress = m_Code.size();

	if (e->GetDoWhile())
	{
		e->GetBody()->Accept(*this);
		e->GetCondition()->Accept(*this);
		PushInstruction(Instruction::Type::JMPT, m_RegisterStack.top(), compareAddress);
	}
	else {
		e->GetCondition()->Accept(*this);
		auto endAddress = PushInstruction(Instruction::Type::JMPF, m_RegisterStack.top(), (size_t)0);
		m_RegisterStack.pop();
		e->GetBody()->Accept(*this);
		PushInstruction(Instruction::Type::JMP, compareAddress);
		m_Code[endAddress].Values.Address[0] = m_Code.size();
	}
}

void ByteCodeGenerator::Visit(IdentifierExpression* e)
{
	m_RegisterStack.push(e->GetName());
}

void ByteCodeGenerator::Visit(LiteralNumber* e)
{
	AddDebugInformation(e);
	IPLString regName = CreateRegister();
	m_RegisterStack.push(regName);

	PushInstruction(Instruction::Type::CONST, regName, e->GetValue());
}

void ByteCodeGenerator::Visit(UnaryExpression* e)
{
	AddDebugInformation(e);
	e->GetExpr()->Accept(*this);
	auto reg = m_RegisterStack.top();
	m_RegisterStack.pop();
	if (e->GetSuffix())
	{
		switch (e->GetOperator())
		{
		case TokenType::PlusPlus:
			PushConst(1);
			{
				auto one = m_RegisterStack.top();
				m_RegisterStack.pop();
				auto o = CreateRegister();
				PushInstruction(Instruction::Type::MOV, o, reg);
				m_RegisterStack.push(o);
				PushInstruction(Instruction::Type::ADD, reg, reg, one);
			}
			return;
		case TokenType::MinusMinus:
			PushConst(1);
			{
				auto one = m_RegisterStack.top();
				m_RegisterStack.pop();
				auto o = CreateRegister();
				PushInstruction(Instruction::Type::MOV, o, reg);
				m_RegisterStack.push(o);
				PushInstruction(Instruction::Type::SUB, reg, reg, one);
			}
			return;
		default:
			NOT_IMPLEMENTED;
			break;
		}
	}
	else
	{
		// prefix
		switch (e->GetOperator())
		{
			case TokenType::Delete:
				NOT_IMPLEMENTED;
				return;
			case TokenType::PlusPlus:
				PushConst(1);
				{
					auto one = m_RegisterStack.top();
					m_RegisterStack.pop();
					m_RegisterStack.push(reg);
					PushInstruction(Instruction::Type::ADD, reg, reg, one);
				}
				return;
			case TokenType::MinusMinus:
				PushConst(1);
				{
					auto one = m_RegisterStack.top();
					m_RegisterStack.pop();
					m_RegisterStack.push(reg);
					PushInstruction(Instruction::Type::SUB, reg, reg, one);
				}
				return;
			case TokenType::Void:
				NOT_IMPLEMENTED;
				return;
			case TokenType::Typeof:
				NOT_IMPLEMENTED;
				return;
			case TokenType::Plus:
				NOT_IMPLEMENTED;
				return;
			case TokenType::Minus:
				NOT_IMPLEMENTED;
				return;
			case TokenType::BitwiseNot:
				NOT_IMPLEMENTED;
				return;
			case TokenType::Bang:
				NOT_IMPLEMENTED;
				return;
		default:
			NOT_IMPLEMENTED;
			break;
		}
	}
}

void ByteCodeGenerator::Visit(LiteralField* e)
{
	e->GetValue()->Accept(*this);
	e->GetIdentifier()->Accept(*this);
}

void ByteCodeGenerator::Visit(LiteralObject* e)
{
	auto initialSize = m_RegisterStack.size();
	auto objReg = CreateRegister();

	for (const auto& field : e->GetValues())
	{
		field->Accept(*this);
		auto fieldName = m_RegisterStack.top();
		m_RegisterStack.pop();

		auto fieldNameReg = CreateRegister();
		PushInstruction(Instruction::Type::STRING, fieldNameReg, fieldName);

		auto valueReg = m_RegisterStack.top();
		m_RegisterStack.pop();
		PushInstruction(Instruction::Type::SET, objReg, fieldNameReg, valueReg);
	}
	assert(m_RegisterStack.size() == initialSize);
	m_RegisterStack.push(objReg);
}

void ByteCodeGenerator::Visit(LiteralString* e)
{
	auto stringReg = CreateRegister();
	PushInstruction(Instruction::Type::STRING, stringReg, e->GetValue());
	m_RegisterStack.push(stringReg);
}

void ByteCodeGenerator::Visit(Call* e)
{
	e->GetObjectOrCall()->Accept(*this);
	if (e->GetMember())
	{
		e->GetMember()->Accept(*this);
	}
}

void ByteCodeGenerator::Visit(MemberAccess* e)
{
	auto objectReg = m_RegisterStack.top();
	m_RegisterStack.pop();

	auto identifierNameReg = CreateRegister();
	PushInstruction(Instruction::Type::STRING, identifierNameReg, e->GetName());

	auto resultReg = CreateRegister();
	PushInstruction(Instruction::Type::GET, objectReg,  identifierNameReg, resultReg);

	m_RegisterStack.push(resultReg);
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
		if (i.Descriptor != ByteCodeGenerator::Instruction::DEBUG)
		{
			result += std::to_string(programCounter) + ": ";
		}
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
			result += "jmp " + std::to_string(i.Values.Address[0]) + '\n';
			break;
		case ByteCodeGenerator::Instruction::JMPT:
			result += "jmpt r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " " + std::to_string(i.Values.Address[0]) + '\n';
			break;
		case ByteCodeGenerator::Instruction::JMPF:
			result += "jmpf r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " " + std::to_string(i.Values.Address[0]) + '\n';
			break;
		case ByteCodeGenerator::Instruction::DUP:
			result += "dup";
			NOT_IMPLEMENTED;
			break;
		case ByteCodeGenerator::Instruction::PUSH:
			result += "push " + std::to_string(i.Values.Int[0]) + '\n';
			break;
		case ByteCodeGenerator::Instruction::POP:
			result += "pop " + std::to_string(i.Values.Int[0]) + '\n';
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
		case ByteCodeGenerator::Instruction::LESSEQ:
			result += "lesseq r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::GREATER:
			result += "greater r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::GREATEREQ:
			result += "greatereq r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::EQ:
			result += "eq r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::NEQ:
			result += "neq r" + std::to_string(ResolveRegisterName(i.Args[0]))
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
			result += "get r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::SET:
			result += "set r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[2])) + '\n';
			break;
		case ByteCodeGenerator::Instruction::INC:
			result += "inc r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(i.Values.Int[0]) + '\n';
			break;
		case ByteCodeGenerator::Instruction::DEC:
			result += "dec r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(i.Values.Int[0]) + '\n';
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
			result += "not r" + std::to_string(ResolveRegisterName(i.Args[0]))
				+ " r" + std::to_string(ResolveRegisterName(i.Args[1])) + '\n';
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
			result += "const r" + std::to_string(ResolveRegisterName(i.Args[0])) + " " + std::to_string(i.Values.Double[0]) + '\n';
			break;
		case ByteCodeGenerator::Instruction::STRING:
			result += "string r" + std::to_string(ResolveRegisterName(i.Args[0])) + " " + i.Args[1] + '\n';
			break;
		case ByteCodeGenerator::Instruction::HALT:
			result += "halt\n";
			break;
		case ByteCodeGenerator::Instruction::DEBUG:
			{
			auto line = int(i.Values.Int[0]);
			auto column = int(i.Values.Int[1]);
			result += "D: " + m_Source[line].substr(0, column) + "@@=>" + m_Source[line].substr(column, m_Source[line].size()) + '\n';
			}
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

IPLString GenerateByteCode(ExpressionPtr program, const IPLString& source, const ByteCodeGeneratorOptions& options)
{
	std::istringstream sourceStream(source);

	IPLVector<IPLString> sourceByLines;
	while (sourceStream.good())
	{
		IPLString currentLine;
		std::getline(sourceStream, currentLine);
		sourceByLines.push_back(currentLine);
	}
	ByteCodeGenerator generator(options, sourceByLines);
	program->Accept(generator);

	return generator.GetCode();
}
