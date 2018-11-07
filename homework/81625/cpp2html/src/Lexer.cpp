#include "Lexer.h"

bool isDigit(char c)
{
    return c >= '0' && c <= '9';
}

bool isLowerCase(char c)
{
    return c >= 'a' && c <= 'z';
}

bool isCapitalCase(char c)
{
    return c >= 'A' && c <= 'Z';
}


Tokenizer::Tokenizer(const char* code) :
    line(1), column(0), codeIndex(0), code(code), state(State::SUCCESS)
{
    keywords["alignas"]         = TokenType::ALIGNAS;
    keywords["alignof"]         = TokenType::ALIGNOF;
    keywords["and"]             = TokenType::AND;
    keywords["and_eq"]          = TokenType::AND_EQ;
    keywords["asm"]             = TokenType::ASM;
    keywords["auto"]            = TokenType::AUTO;
    keywords["bitand"]          = TokenType::BITAND;
    keywords["bitor"]           = TokenType::BITOR;
    keywords["bool"]            = TokenType::BOOL;
    keywords["break"]           = TokenType::BREAK;
    keywords["case"]            = TokenType::CASE;
    keywords["catch"]           = TokenType::CATCH;
    keywords["char"]            = TokenType::CHAR;
    keywords["char16_t"]        = TokenType::CHAR16_T;
    keywords["char32_t"]        = TokenType::CHAR32_T;
    keywords["class"]           = TokenType::CLASS;
    keywords["compl"]           = TokenType::COMPL;
    keywords["const"]           = TokenType::CONST;
    keywords["constexpr"]       = TokenType::CONSTEXPR;
    keywords["const_cast"]      = TokenType::CONST_CAST;
    keywords["continue"]        = TokenType::CONTINUE;
    keywords["decltype"]        = TokenType::DECLTYPE;
    keywords["default"]         = TokenType::DEFAULT;
    keywords["delete"]          = TokenType::DELETE;
    keywords["do"]              = TokenType::DO;
    keywords["double"]          = TokenType::DOUBLE;
    keywords["dynamic_cast"]    = TokenType::DYNAMIC_CAST;
    keywords["else"]            = TokenType::ELSE;
    keywords["enum"]            = TokenType::ENUM;
    keywords["explicit"]        = TokenType::EXPLICIT;
    keywords["export"]          = TokenType::EXPORT;
    keywords["extern"]          = TokenType::EXTERN;
    keywords["false"]           = TokenType::FALSE;
    keywords["float"]           = TokenType::FLOAT;
    keywords["for"]             = TokenType::FOR;
    keywords["friend"]          = TokenType::FRIEND;
    keywords["goto"]            = TokenType::GOTO;
    keywords["if"]              = TokenType::IF;
    keywords["inline"]          = TokenType::INLINE;
    keywords["int"]             = TokenType::INT;
    keywords["long"]            = TokenType::LONG;
    keywords["mutable"]         = TokenType::MUTABLE;
    keywords["namespace"]       = TokenType::NAMESPACE;
    keywords["new"]             = TokenType::NEW;
    keywords["noexcept"]        = TokenType::NOEXCEPT;
    keywords["not"]             = TokenType::NOT;
    keywords["not_eq"]          = TokenType::NOT_EQ;
    keywords["nullptr"]         = TokenType::NULLPTR;
    keywords["operator"]        = TokenType::OPERATOR;
    keywords["or"]              = TokenType::OR;
    keywords["or_eq"]           = TokenType::OR_EQ;
    keywords["private"]         = TokenType::PRIVATE;
    keywords["protected"]       = TokenType::PROTECTED;
    keywords["public"]          = TokenType::PUBLIC;
    keywords["register"]        = TokenType::REGISTER;
    keywords["reinterpret_cast"]= TokenType::REINTERPRET_CAST;
    keywords["return"]          = TokenType::RETURN;
    keywords["short"]           = TokenType::SHORT;
    keywords["signed"]          = TokenType::SIGNED;
    keywords["sizeof"]          = TokenType::SIZEOF;
    keywords["static"]          = TokenType::STATIC;
    keywords["static_assert"]   = TokenType::STATIC_ASSERT;
    keywords["static_cast"]     = TokenType::STATIC_CAST;
    keywords["struct"]          = TokenType::STRUCT;
    keywords["switch"]          = TokenType::SWITCH;
    keywords["template"]        = TokenType::TEMPLATE;
    keywords["this"]            = TokenType::THIS;
    keywords["thread_local"]    = TokenType::THREAD_LOCAL;
    keywords["throw"]           = TokenType::THROW;
    keywords["true"]            = TokenType::TRUE;
    keywords["try"]             = TokenType::TRY;
    keywords["typedef"]         = TokenType::TYPEDEF;
    keywords["typeid"]          = TokenType::TYPEID;
    keywords["typename"]        = TokenType::TYPENAME;
    keywords["union"]           = TokenType::UNION;
    keywords["unsigned"]        = TokenType::UNSIGNED;
    keywords["using"]           = TokenType::USING;
    keywords["virtual"]         = TokenType::VIRTUAL;
    keywords["void"]            = TokenType::VOID;
    keywords["volatile"]        = TokenType::VOLATILE;
    keywords["wchar_t"]         = TokenType::WCHAR_T;
    keywords["while"]           = TokenType::WHILE;
    keywords["xor"]             = TokenType::XOR;
    keywords["xor_eq"]          = TokenType::XOR_EQ;
    
    preprocessors["#define"]    = TokenType::PRE_DEFINE;
    preprocessors["#undef"]     = TokenType::PRE_UNDEF;
    preprocessors["#include"]   = TokenType::PRE_INCLUDE;
    preprocessors["#if"]        = TokenType::PRE_IF;
    preprocessors["#ifdef"]     = TokenType::PRE_IFDEF;
    preprocessors["#ifndef"]    = TokenType::PRE_IFNDEF;
    preprocessors["#else"]      = TokenType::PRE_ELSE;
    preprocessors["#elif"]      = TokenType::PRE_ELIF;
    preprocessors["#endif"]     = TokenType::PRE_ENDIF;
    preprocessors["#line"]      = TokenType::PRE_LINE;
    preprocessors["#error"]     = TokenType::PRE_ERROR;
    preprocessors["#pragma"]    = TokenType::PRE_PRAGMA;
}

bool Tokenizer::isAtEOF() const
{
    return code[codeIndex] == '\0';
}

bool Tokenizer::match(char c) const
{
    return code[codeIndex] == c;
}

bool Tokenizer::isStateError() const
{
    return state == State::ERROR;
}

bool Tokenizer::isStateSuccess() const
{
    return state == State::SUCCESS;
}

bool Tokenizer::tokenize(std::vector<Token> &tokens)
{
    Token token;
    do
    {
        token = nextToken();
        if(token.type == TokenType::INVALID)
        {
            return false;
        }
        tokens.push_back(token);

    } while(token.type != TokenType::Eof);

    return true;
}

void Tokenizer::skipWhiteSpaces()
{
    while(true)
    {
        if(code[codeIndex] == ' ' || code[codeIndex] == '\t')
        {
            column++;
        }
        else if(code[codeIndex] == '\n')
        {
            column = 0;
            line++;
        }
        else
        {
            break;
        }
        codeIndex++;
    }
}

void Tokenizer::nextSymbol()
{
    codeIndex++;
    column++;
}

void Tokenizer::prevSymbol()
{
    codeIndex--;
    column--;
}

double Tokenizer::parseNumber()
{
    try
    {
        size_t bytes = 0;
        double d = std::stod(code + codeIndex, &bytes);
        codeIndex += static_cast<unsigned>(bytes);
        column += static_cast<unsigned>(bytes);
        state = State::SUCCESS;
        return d;
    }
    catch(std::invalid_argument &e)
    {
        state = State::FAIL;
        return 0;
    }
    catch(std::out_of_range &e)
    {
        state = State::ERROR;
        return 0;
    }
}

std::string Tokenizer::parseComment()
{
    if(code[codeIndex] == '/')
    {
        unsigned start = codeIndex;
        nextSymbol();
        if(code[codeIndex] == '/')
        {
            nextSymbol();
            while(code[codeIndex] != '\n')
            {
                nextSymbol();
            }
            state = State::SUCCESS;
            return std::string(code, start, codeIndex-start);
        }
        if(code[codeIndex] == '*')
        {
            nextSymbol();
            bool flag = true;
            while(flag)
            {
                while(code[codeIndex] != '*' && !isAtEOF())
                {
                    nextSymbol();
                }

                if(isAtEOF())
                {
                    state = State::ERROR;
                    return "";
                }

                nextSymbol();
                if(code[codeIndex] == '/')
                {
                    nextSymbol();
                    flag = false;
                }
            }
            state = State::SUCCESS;
            return std::string(code, start, codeIndex-start);
        }
    }

    state = State::FAIL;
    return "";
}

std::string Tokenizer::parseString()
{
    // not a string
    if(code[codeIndex] != '\'' && code[codeIndex] != '"' )
    {
        state = State::FAIL;
        return "";
    }

    const char bound = code[codeIndex];
    const unsigned start = codeIndex;

    nextSymbol();
    while(true)
    {
        while(code[codeIndex] != bound && !isAtEOF() && code[codeIndex] != '\n')
        {
            nextSymbol();
        }

        if(code[codeIndex] == bound)
        {
            // escaping character
            if(code[codeIndex-1] != '\\')
            {
                nextSymbol();
                state = State::SUCCESS;
                return std::string(code, start, codeIndex-start);
            }
            else
            {
                codeIndex++;
            }
        }
        else break;
    }

    // not a correct string
    state = State::ERROR;
    return "";
}

Token Tokenizer::parseKeyword()
{
    if(!isLowerCase(code[codeIndex]))
    {
        state = State::FAIL;
        return produceToken(TokenType::INVALID);
    }

    const unsigned start = codeIndex;
    nextSymbol();
    while(isLowerCase(code[codeIndex]) || isDigit(code[codeIndex]) || code[codeIndex] == '_')
    {
        nextSymbol();
    }

    const std::string key = std::string(code, start, codeIndex-start);
    const auto keyword = keywords.find(key);

    if(keyword == keywords.end())
    {
        column -= codeIndex-start;
        codeIndex = start;
        state = State::FAIL;
        return produceToken(TokenType::INVALID);
    }

    state = State::SUCCESS;
    return produceToken(keyword->second, keyword->first);
}

Token Tokenizer::parsePreprocessor()
{
    // not a preprocessor
    if(code[codeIndex] != '#')
    {
        state = State::FAIL;
        return produceToken(TokenType::INVALID);
    }
    
    const unsigned start = codeIndex;
    nextSymbol();
    while(isLowerCase(code[codeIndex]))
    {
        nextSymbol();
    }
    
    const std::string key = std::string(code, start, codeIndex-start);
    const auto preproc = preprocessors.find(key);
    
    if(preproc == preprocessors.end())
    {
        column -= codeIndex-start;
        codeIndex = start;
        state = State::FAIL;
        return produceToken(TokenType::INVALID);
    }
    state = State::SUCCESS;
    return produceToken(preproc->second, preproc->first);
}

std::string Tokenizer::parseIdentifier()
{
    if(!isLowerCase(code[codeIndex]) && !isCapitalCase(code[codeIndex]) && code[codeIndex] != '_')
    {
        state = State::FAIL;
        return "";
    }

    const unsigned start = codeIndex;

    nextSymbol();
    while(isLowerCase(code[codeIndex]) || isCapitalCase(code[codeIndex]) ||
            isDigit(code[codeIndex]) || code[codeIndex] == '_')
    {
        nextSymbol();
    }

    state = State::SUCCESS;
    return std::string(code, start, codeIndex-start);
}

Token Tokenizer::produceToken(TokenType type, std::string lexeme, double val)
{
    return Token{type, line, lexeme, val};
}

Token Tokenizer::produceToken(TokenType type)
{
    switch(type)
    {
        case TokenType::LEFT_PAREN:            return produceToken(type, "(");
        case TokenType::RIGHT_PAREN:           return produceToken(type, ")");
        case TokenType::LEFT_BRACKET:          return produceToken(type, "[");
        case TokenType::RIGHT_BRACKET:         return produceToken(type, "]");
        case TokenType::LEFT_CUR_BRAC:         return produceToken(type, "{");
        case TokenType::RIGHT_CUR_BRAC:        return produceToken(type, "}");
        case TokenType::PLUS:                  return produceToken(type, "+");
        case TokenType::MINUS:                 return produceToken(type, "-");
        case TokenType::MULTIPLICATION:        return produceToken(type, "*");
        case TokenType::DIVIDE:                return produceToken(type, "/");
        case TokenType::MODULO:                return produceToken(type, "%");
        case TokenType::LESSER:                return produceToken(type, "<");
        case TokenType::GREATER:               return produceToken(type, ">");
        case TokenType::ASSIGN:                return produceToken(type, "=");
        case TokenType::SEMI_COLON:            return produceToken(type, ";");
        case TokenType::QUESTION_MARK:         return produceToken(type, "?");
        case TokenType::EXCLAMATION_MARK:      return produceToken(type, "!");
        case TokenType::COLON:                 return produceToken(type, ":");
        case TokenType::BITWISE_AND:           return produceToken(type, "&");
        case TokenType::BITWISE_OR:            return produceToken(type, "|");
        case TokenType::BITWISE_XOR:           return produceToken(type, "^");
        case TokenType::BITWISE_NOT:           return produceToken(type, "~");
        case TokenType::DOT:                   return produceToken(type, ".");
        case TokenType::SCOPE:                 return produceToken(type, "::");
        case TokenType::PLUS_PLUS:             return produceToken(type, "++");
        case TokenType::MINUS_MINUS:           return produceToken(type, "--");
        case TokenType::PTR_ARROW:             return produceToken(type, "->");
        case TokenType::LEFT_BITWISE_SHIFT:    return produceToken(type, "<<");
        case TokenType::RIGHT_BITWISE_SHIFT:   return produceToken(type, ">>");
        case TokenType::EQUAL:                 return produceToken(type, "==");
        case TokenType::NOT_EQUAL:             return produceToken(type, "!=");
        case TokenType::LESS_EQUAL:            return produceToken(type, "<=");
        case TokenType::GREATER_EQUAL:         return produceToken(type, ">=");
        case TokenType::LOGIC_AND:             return produceToken(type, "&&");
        case TokenType::LOGIC_OR:              return produceToken(type, "||");
        case TokenType::MOD_EQUAL:             return produceToken(type, "%=");
        case TokenType::MULT_EQUAL:            return produceToken(type, "*=");
        case TokenType::DIV_EQUAL:             return produceToken(type, "/=");
        case TokenType::PLUS_EQUAL:            return produceToken(type, "+=");
        case TokenType::MINUS_EQUAL:           return produceToken(type, "-=");
        case TokenType::BIT_AND_EQUAL:         return produceToken(type, "&=");
        case TokenType::BIT_OR_EQUAL:          return produceToken(type, "|=");
        case TokenType::BIT_XOR_EQUAL:         return produceToken(type, "^=");
        case TokenType::LEFT_SHIFT_EQUAL:      return produceToken(type, "<<=");
        case TokenType::RIGHT_SHIFT_EQUAL:     return produceToken(type, ">>=");
        default: return produceToken(type, "");
    }
}

Token Tokenizer::nextToken()
{
    if(isAtEOF())
    {
        return produceToken(TokenType::Eof);
    }

    skipWhiteSpaces();

    // comments
    std::string comment = parseComment();
    if(isStateSuccess())
    {
        return produceToken(TokenType::COMMENT, comment);
    }
    
    // preprocessors
    Token preproc = parsePreprocessor();
    if(isStateSuccess())
    {
        return preproc;
    }

    switch(code[codeIndex])
    {
        case '(':
            nextSymbol();
            return produceToken(TokenType::LEFT_PAREN);
        case ')':
            nextSymbol();
            return produceToken(TokenType::RIGHT_PAREN);
        case '[':
            nextSymbol();
            return produceToken(TokenType::LEFT_BRACKET);
        case ']':
            nextSymbol();
            return produceToken(TokenType::RIGHT_BRACKET);
        case '{':
            nextSymbol();
            return produceToken(TokenType::LEFT_CUR_BRAC);
        case '}':
            nextSymbol();
            return produceToken(TokenType::RIGHT_CUR_BRAC);
        case '+':
            nextSymbol();
            if(match('+'))
            {
                nextSymbol();
                return produceToken(TokenType::PLUS_PLUS);
            }
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::PLUS_EQUAL);
            }
            return produceToken(TokenType::PLUS);
        case '-':
            nextSymbol();
            if(match('-'))
            {
                nextSymbol();
                return produceToken(TokenType::MINUS_MINUS);
            }
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::MINUS_EQUAL);
            }
            if(match('>'))
            {
                nextSymbol();
                return produceToken(TokenType::PTR_ARROW);
            }
            return produceToken(TokenType::MINUS);
        case '*':
            nextSymbol();
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::MULT_EQUAL);
            }
            return produceToken(TokenType::MULTIPLICATION);
        case '/':
            nextSymbol();
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::DIV_EQUAL);
            }
            return produceToken(TokenType::DIVIDE);
        case '%':
            nextSymbol();
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::MOD_EQUAL);
            }
            return produceToken(TokenType::MODULO);
        case '<':
            nextSymbol();
            if(match('<'))
            {
                nextSymbol();
                if(match('='))
                {
                    nextSymbol();
                    return produceToken(TokenType::LEFT_SHIFT_EQUAL);
                }
                return produceToken(TokenType::LEFT_BITWISE_SHIFT);
            }
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::LESS_EQUAL);
            }
            return produceToken(TokenType::LESSER);
        case '>':
            nextSymbol();
            if(match('>'))
            {
                nextSymbol();
                if(match('='))
                {
                    nextSymbol();
                    return produceToken(TokenType::RIGHT_SHIFT_EQUAL);
                }
                return produceToken(TokenType::RIGHT_BITWISE_SHIFT);
            }
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::GREATER_EQUAL);
            }
            return produceToken(TokenType::GREATER);
        case '=':
            nextSymbol();
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::EQUAL);
            }
            return produceToken(TokenType::ASSIGN);
        case ';':
            nextSymbol();
            return produceToken(TokenType::SEMI_COLON);
        case '?':
            nextSymbol();
            return produceToken(TokenType::QUESTION_MARK);
        case '!':
            nextSymbol();
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::NOT_EQUAL);
            }
            return produceToken(TokenType::EXCLAMATION_MARK);
        case ':':
            nextSymbol();
            if(match(':'))
            {
                nextSymbol();
                return produceToken(TokenType::SCOPE);
            }
            return produceToken(TokenType::COLON);
        case '&':
            nextSymbol();
            if(match('&'))
            {
                nextSymbol();
                return produceToken(TokenType::LOGIC_AND);
            }
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::BIT_AND_EQUAL);
            }
            return produceToken(TokenType::BITWISE_AND);
        case '|':
            nextSymbol();
            if(match('|'))
            {
                nextSymbol();
                return produceToken(TokenType::LOGIC_AND);
            }
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::BIT_OR_EQUAL);
            }
            return produceToken(TokenType::BITWISE_OR);
        case '^':
            nextSymbol();
            if(match('='))
            {
                nextSymbol();
                return produceToken(TokenType::BIT_XOR_EQUAL);
            }
            return produceToken(TokenType::BITWISE_XOR);
        case '~':
            nextSymbol();
            return produceToken(TokenType::BITWISE_NOT);
        case '.':
            nextSymbol();
            return produceToken(TokenType::DOT);
        default: break;
    }

    // number literals
    double num = parseNumber();
    if(isStateSuccess())
    {
        return produceToken(TokenType::NUMBER, "", num);
    }
    else if(isStateError())
    {
        return produceToken(TokenType::INVALID);
    }

    // string literals
    std::string lex = parseString();
    if(isStateSuccess())
    {
        return produceToken(TokenType::STRING, lex);
    }
    else if(isStateError())
    {
        return produceToken(TokenType::INVALID);
    }

    // keywords
    Token tkn = parseKeyword();
    if(isStateSuccess())
    {
        return tkn;
    }

    // identifiers
    std::string identifier = parseIdentifier();
    if(isStateSuccess())
    {
        return produceToken(TokenType::IDENTIFIER, identifier);
    }

    return produceToken(TokenType::INVALID);
}

