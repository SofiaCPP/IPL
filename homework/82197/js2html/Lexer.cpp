#include "Lexer.h"


Tokenizer::Tokenizer(const char* code, const LexerSettings& settings)
	: m_Line(0)
	, m_Column(0)
	, m_Current(0)
	, m_LastTokenPozition(0)
	, m_Code(code)
	, m_Error()
	, m_GenerationState(State::Success)
	, m_Settings(settings)
{
	m_KeyWordsTable["await"] = TokenType::Await;
	m_KeyWordsTable["break"] = TokenType::Break;
	m_KeyWordsTable["case"] = TokenType::Case;
	m_KeyWordsTable["catch"] = TokenType::Catch;
	m_KeyWordsTable["class"] = TokenType::Class;
	m_KeyWordsTable["const"] = TokenType::Const;
	m_KeyWordsTable["continue"] = TokenType::Continue;
	m_KeyWordsTable["debugger"] = TokenType::Debugger;
	m_KeyWordsTable["default"] = TokenType::Default;
	m_KeyWordsTable["delete"] = TokenType::Delete;
	m_KeyWordsTable["do"] = TokenType::Do;
	m_KeyWordsTable["else"] = TokenType::Else;
	m_KeyWordsTable["enum"] = TokenType::Enum;
	m_KeyWordsTable["export"] = TokenType::Export;
	m_KeyWordsTable["extends"] = TokenType::Extends;
	m_KeyWordsTable["false"] = TokenType::False;
	m_KeyWordsTable["finally"] = TokenType::Finally;
	m_KeyWordsTable["for"] = TokenType::For;
	m_KeyWordsTable["function"] = TokenType::Function;
	m_KeyWordsTable["if"] = TokenType::If;
	m_KeyWordsTable["implements"] = TokenType::Implements;
	m_KeyWordsTable["import"] = TokenType::Import;
	m_KeyWordsTable["in"] = TokenType::In;
	m_KeyWordsTable["instanceof"] = TokenType::Instanceof;
	m_KeyWordsTable["interface"] = TokenType::Interface;
	m_KeyWordsTable["let"] = TokenType::Let;
	m_KeyWordsTable["new"] = TokenType::New;
	m_KeyWordsTable["null"] = TokenType::Null;
	m_KeyWordsTable["package"] = TokenType::Package;
	m_KeyWordsTable["private"] = TokenType::Private;
	m_KeyWordsTable["protected"] = TokenType::Protected;
	m_KeyWordsTable["public"] = TokenType::Public;
	m_KeyWordsTable["return"] = TokenType::Return;
	m_KeyWordsTable["super"] = TokenType::Super;
	m_KeyWordsTable["switch"] = TokenType::Switch;
	m_KeyWordsTable["static"] = TokenType::Static;
	m_KeyWordsTable["this"] = TokenType::This;
	m_KeyWordsTable["throw"] = TokenType::Throw;
	m_KeyWordsTable["try"] = TokenType::Try;
	m_KeyWordsTable["true"] = TokenType::True;
	m_KeyWordsTable["var"] = TokenType::Var;
	m_KeyWordsTable["void"] = TokenType::Void;
	m_KeyWordsTable["while"] = TokenType::While;
	m_KeyWordsTable["with"] = TokenType::With;
	m_KeyWordsTable["yield"] = TokenType::Yield;
}

LexerResult Tokenizer::Tokenize()
{
	LexerResult result{ true, IPLError(), IPLVector<Token>() };
	Token token;
	do
	{
		token = NextToken();

		if (m_GenerationState == State::Error)
		{
			result.IsSuccessful = false;
			result.Error = IPLError(m_Error);
			return result;
		}
		if (FilterToken(token.Type))
		{
			result.tokens.emplace_back(token);
		}
	} while (token.Type != TokenType::Eof  && token.Type != TokenType::Invalid);

	return result;
}

Token Tokenizer::NextToken()
{
	if (IsEnd(m_Code[m_Current]))
	{
		return ProduceToken(TokenType::Eof);
	}

	IPLString comment = ParseComment();
	if (IsStateError())
	{
		return ProduceErrorToken();
	}
	if (IsStateSuccess())
	{
		return ProduceCommentToken(TokenType::Comment, comment);
	}

	// Single Char
    char currentChar = m_Code[m_Current];
    NextSymbol();
	switch (currentChar)
	{
	case '(': return ProduceToken(TokenType::LeftParen);
	case ')': return ProduceToken(TokenType::RightParen);
	case '{': return ProduceToken(TokenType::LeftBrace);
	case '}': return ProduceToken(TokenType::RightBrace);
	case ',': return ProduceToken(TokenType::Comma);
	case '.': return ProduceToken(TokenType::Dot);
	case '-': return Match('-') ? ProduceToken(TokenType::MinusMinus) : Match('=') ? ProduceToken(TokenType::MinusEqual) : ProduceToken(TokenType::Minus);
	case '+': return Match('+') ? ProduceToken(TokenType::PlusPlus) : Match('=') ? ProduceToken(TokenType::PlusEqual) : ProduceToken(TokenType::Plus);
	case ';': return ProduceToken(TokenType::Semicolon);
	case '*': return Match('*') ? Match('=') ? ProduceToken(TokenType::StarStarEqual) : ProduceToken(TokenType::StarStar) : ProduceToken(TokenType::Star);
	case '/': return Match('=') ? ProduceToken(TokenType::DivisionEqual) : ProduceToken(TokenType::Division);
	case '%': return Match('=') ? ProduceToken(TokenType::ModuloEqual) : ProduceToken(TokenType::Modulo);
	case '~': return ProduceToken(TokenType::BitwiseNot);
	case '=': return Match('=') ? Match('=') ? ProduceToken(TokenType::StrictEqual) : ProduceToken(TokenType::EqualEqual) : Match('>') ? ProduceToken(TokenType::Arrow) : ProduceToken(TokenType::Equal);
	case '!': return Match('=') ? Match('=') ? ProduceToken(TokenType::StrictNotEqual) : ProduceToken(TokenType::NotEqual) : ProduceToken(TokenType::LogicalNot);
	case '>':
        if (Match('>')) // >>
        {
            if (Match('>')) // >>>
            {
                if (Match('=')) // >>>=
                {
                    return ProduceToken(TokenType::UnsignedRightShiftEqual);
                }

                return ProduceToken(TokenType::UnsignedRightShift);
            }

            if (Match('=')) // >>=
            {
                return ProduceToken(TokenType::RightShiftEqual);
            }

            return ProduceToken(TokenType::RightShift);
        }

        if (Match('=')) // >=
        {
            return ProduceToken(TokenType::GreaterEqual);
        }

        return ProduceToken(TokenType::Greater);
	case '<': return Match('=') ? ProduceToken(TokenType::LessEqual) : Match('<') ? ProduceToken(TokenType::LeftShift) : ProduceToken(TokenType::Less);
	case '&': return Match('&') ? ProduceToken(TokenType::LogicalAnd) : Match('=') ? ProduceToken(TokenType::BitwiseAndEqual) : ProduceToken(TokenType::BitwiseAnd);
	case '^': return Match('=') ? ProduceToken(TokenType::BitwiseXorEqual) : ProduceToken(TokenType::BitwiseXor);
	case '|': return Match('|') ? ProduceToken(TokenType::LogicalOr) : Match('=') ? ProduceToken(TokenType::BitwiseOrEqual) : ProduceToken(TokenType::BitwiseOr);
	case '?': return ProduceToken(TokenType::QuestionMark);
	case ':': return ProduceToken(TokenType::Colon);
	case '[': return ProduceToken(TokenType::LeftSquareBracket);
	case ']': return ProduceToken(TokenType::RightSquareBracket);
	case '\\': return ProduceToken(TokenType::Backslash);
	case '\n': return ProduceToken(TokenType::NewLine);
	case ' ': return ProduceToken(TokenType::Whitespace);
	case '\t': return ProduceToken(TokenType::Tab);
	default:
        PreviousSymbol();
		break;
	}

	double number = ParseNumber();
	if (IsStateSuccess())
	{
		return ProduceToken(TokenType::Number, "", number);
	}

	IPLString string = ParseString();
	if (IsStateSuccess())
	{
		return ProduceStringToken(TokenType::String, string);
	}
	else if (IsStateError())
	{
		return ProduceErrorToken();
	}

	Keyword keyword = ParseKeyword();
	if (IsStateSuccess())
	{
		return ProduceKeywordToken(keyword.Type, keyword.Content);
	}

	Identifier identifier = ParseIdentifier();
	if (IsStateSuccess())
	{
		return ProduceIdentifierToken(identifier.Type, identifier.Content);
	}

	return ProduceInvalidToken();
}

bool Tokenizer::FilterToken(TokenType type)
{
	if (type == TokenType::Comment)
	{
		return m_Settings.CreateCommentTokens;
	}
	else if (type == TokenType::Whitespace ||
		type == TokenType::Tab ||
		type == TokenType::NewLine)
	{
		return m_Settings.CreateWhitespaceTokens;
	}
	return true;
}

bool Tokenizer::Match(const char c)
{
	if (c == m_Code[m_Current])
	{
		NextSymbol();
		return true;
	}
	return false;
}

IPLString Tokenizer::ParseComment()
{
	unsigned int start = m_Current;

	if (!Match('/'))
	{
        m_GenerationState = State::Fail;
        return IPLString();
	}

	if (Match('/'))
	{
		while (!IsEnd(m_Code[m_Current]) && !IsNewLine(m_Code[m_Current]))
		{
			NextSymbol();
		}

        m_GenerationState = State::Success;
        return IPLString(m_Code + start, m_Code + m_Current);
	}
	else if (!Match('*'))
	{
		PreviousSymbol();
		m_GenerationState = State::Fail;
        return IPLString();
	}

	while (!IsEnd(m_Code[m_Current]))
	{
		if (Match('*') && Match('/'))
		{
            m_GenerationState = State::Success;
            return IPLString(m_Code + start, m_Code + m_Current);
		}
		NextSymbol();
	}

	SetError("unterminated comment");

	return IPLString();
}

Identifier Tokenizer::ParseIdentifier()
{
	if (!IsValidIdentifierStartingChar(m_Code[m_Current]))
	{
        m_GenerationState = State::Fail;
        return Identifier();
	}

	auto start = m_Current;

	NextSymbol();
	while (IsValidIdentifierChar(m_Code[m_Current]))
	{
		NextSymbol();
	}

    m_GenerationState = State::Success;
    return Identifier{TokenType::Identifier, IPLString(m_Code + start, m_Code + m_Current)};
}

Keyword Tokenizer::ParseKeyword()
{
	if (!IsLowerCase(m_Code[m_Current]))
	{
        m_GenerationState = State::Fail;
        return Keyword();
	}

	auto start = m_Current;

	NextSymbol();
	while (IsLowerCase(m_Code[m_Current]))
	{
		NextSymbol();
	}

	IPLString key = IPLString(m_Code + start, m_Code + m_Current);
	auto keyword = m_KeyWordsTable.find(key);

	if (keyword == m_KeyWordsTable.end())
	{
		// It's not a keyword so we must revert current counter
		m_Column -= m_Current - start;
		m_Current = start;

		m_GenerationState = State::Fail;
        return Keyword();
	}

    m_GenerationState = State::Success;
    return Keyword{keyword->second, keyword->first};
}

double Tokenizer::ParseNumber()
{
	if (!IsDigit(m_Code[m_Current]))
	{
		m_GenerationState = State::Fail;
        return 0.0;
	}

	size_t parsedBytes = 0;
	double number = std::stod(m_Code + m_Current, &parsedBytes);
	m_Current += static_cast<unsigned>(parsedBytes);
	m_Column += static_cast<unsigned>(parsedBytes);

    m_GenerationState = State::Success;
    return number;
}

IPLString Tokenizer::ParseString()
{
	if (!IsStringBound(m_Code[m_Current]))
	{
		m_GenerationState = State::Fail;
        return IPLString();
	}

	char bound = m_Code[m_Current];
	unsigned int start = m_Current;

	// skip first " or '
	NextSymbol();
	while ((m_Code[m_Current] != bound || m_Code[m_Current-1] == '\\') && !IsEnd(m_Code[m_Current]) && !IsNewLine(m_Code[m_Current]))
	{
		NextSymbol();
	}

	if (IsEnd(m_Code[m_Current]) || IsNewLine(m_Code[m_Current]))
	{
		SetError("\"\" string literal contains an unescaped line break");
        
        return IPLString();
	}

	// skip second " or '
	NextSymbol();

    m_GenerationState = State::Success;
    return IPLString(m_Code + start, m_Code + m_Current);
}

LexerResult Tokenize(const char* code, const LexerSettings& settings)
{
	Tokenizer tokenizer(code, settings);
	return tokenizer.Tokenize();
}

LexerResult Tokenize(const char * code)
{
	return Tokenize(code, { false, false });
}