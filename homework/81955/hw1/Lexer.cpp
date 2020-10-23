#include "Lexer.h"

inline bool Helper::isCapitalLetter(char c)
{
    return c >= 'A' && c <= 'Z';
}
inline bool Helper::isSmallLetter(char c)
{
    return c >= 'a' && c <= 'z';
}

inline bool Helper::isLetter(char c)
{
    return isCapitalLetter(c) || isSmallLetter(c);
}

inline bool Helper::isValidIdentifierStart(char c)
{
    return Helper::isLetter(c) || c == '_';
}

inline bool Helper::isValidIdentifierLetter(char c)
{
    return isValidIdentifierStart(c) || isDigit(c);
}

inline bool Helper::isDigit(char c)
{
    return c >= '0' && c <= '9';
}
inline bool Helper::isEof(char c)
{
    return c == '\0';
}
inline bool Helper::isNewLine(char c)
{
    return c == '\n';
}
inline bool Helper::isHexLiteral(char c)
{
    return c == 'x';
}
inline bool Helper::isBinLiteral(char c)
{
    return c == 'b';
}
inline bool Helper::isDot(char c)
{
    return c == '.';
}
inline bool Helper::isOctalLiteral(char c)
{
    return c == 'o';
}

inline bool Helper::isStringLiteral(char c)
{
    return c == '\"' || c == '`';
}

inline bool Helper::isBinDigit(char c)
{
    return c == '0' || c == '1';
}
inline bool Helper::isOctalDigit(char c)
{
    return isBinDigit(c) || c >= '0' && c <= '7';
}

inline bool Helper::isHexDigit(char c)
{
    return isDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

Tokenizer::Tokenizer(const char *_code) : code(_code), index(0), state(true)
{
    keywordTable["break"] = TokenType::Break;
    keywordTable["case"] = TokenType::Case;
    keywordTable["chan"] = TokenType::Chan;
    keywordTable["const"] = TokenType::Const;
    keywordTable["continue"] = TokenType::Continue;
    keywordTable["default"] = TokenType::Default;
    keywordTable["defer"] = TokenType::Defer;
    keywordTable["else"] = TokenType::Else;
    keywordTable["fallthrough"] = TokenType::Fallthrough;
    keywordTable["for"] = TokenType::For;
    keywordTable["func"] = TokenType::Func;
    keywordTable["go"] = TokenType::Go;
    keywordTable["goto"] = TokenType::GoTo;
    keywordTable["if"] = TokenType::If;
    keywordTable["import"] = TokenType::Import;
    keywordTable["Interface"] = TokenType::Interface;
    keywordTable["map"] = TokenType::Map;
    keywordTable["package"] = TokenType::Package;
    keywordTable["range"] = TokenType::Range;
    keywordTable["return"] = TokenType::Return;
    keywordTable["select"] = TokenType::Select;
    keywordTable["struct"] = TokenType::Struct;
    keywordTable["switch"] = TokenType::Switch;
    keywordTable["type"] = TokenType::Type;
    keywordTable["var"] = TokenType::Var;
    keywordTable["nil"] = TokenType::Nil;
    keywordTable["true"] = TokenType::True;
    keywordTable["false"] = TokenType::False;
    keywordTable["int"] = TokenType::Int;
    keywordTable["string"] = TokenType::String;
    keywordTable["float"] = TokenType::Float;
    keywordTable["bool"] = TokenType::Bool;
}
inline bool Tokenizer::match(char c)
{
    if (code[index] == c)
    {
        nextSymbol();
        return true;
    }
    return false;
}

inline void Tokenizer::nextSymbol()
{
    index++;
}

inline void Tokenizer::previousSymbol()
{
    index--;
}

std::string Tokenizer::parseWhitespace()
{
    std::string whitespace;
    while (code[index] == ' ' || code[index] == '\n' || code[index] == '\t')
    {
        whitespace += code[index];
        nextSymbol();
    }
    if (whitespace != "")
        state = true;
    else
        state = false;
    return whitespace;
}

std::string Tokenizer::parseComment()
{
    if (!match('/'))
    {
        state = false;
        return "";
    }

    std::string comment = "/";

    if (match('/'))
    {
        comment += '/';
        while (!Helper::isEof(code[index]) && !Helper::isNewLine(code[index]))
        {
            comment += code[index];
            nextSymbol();
        }
        state = true;
        return comment;
    }

    else if (match('*'))
    {
        comment += '*';
        while (code[index] != '*' && code[index + 1] != '/' && !Helper::isEof(code[index]))
        {
            comment += code[index];
            nextSymbol();
        }
        if (!Helper::isEof(code[index]))
        {
            nextSymbol();
            nextSymbol();
            comment += "*/";
        }
        state = true;
        return comment;
    }
    else
        previousSymbol();

    state = false;
    return "";
}

std::string Tokenizer::parseString()
{

    if (!Helper::isStringLiteral(code[index]))
    {
        state = false;
        return "";
    }

    char c = code[index];
    std::string res;
    res += c;
    nextSymbol();
    while (!match(c) && !Helper::isEof(code[index]))
    {
        res += code[index];
        nextSymbol();
    }
    if (code[index - 1] == c)
    {
        res += c;
    }
    state = true;
    return res;
}

std::string Tokenizer::parseNumber()
{
    char c = code[index];
    if (!Helper::isDigit(c))
    {
        state = false;
        return "";
    }

    nextSymbol();
    std::string res;
    res += c;

    if (Helper::isBinLiteral(code[index]))
    {
        res += code[index];
        nextSymbol();
        while (Helper::isBinDigit(code[index]) && !Helper::isEof(code[index]))
        {
            res += code[index];
            nextSymbol();
        }
        if (Helper::isBinLiteral(code[index - 1]))
        {
            state = false;
            return res;
        }
        state = true;
        return res;
    }

    if (Helper::isHexLiteral(code[index]))
    {
        res += code[index];
        nextSymbol();
        while (Helper::isHexDigit(code[index]) && !Helper::isEof(code[index]))
        {
            res += code[index];
            nextSymbol();
        }
        if (Helper::isHexLiteral(code[index - 1]))
        {
            state = false;
            return res;
        }
        state = true;
        return res;
    }

    if (Helper::isOctalLiteral(code[index]))
    {
        res += code[index];
        nextSymbol();
        while (Helper::isOctalDigit(code[index]) && !Helper::isEof(code[index]))
        {
            res += code[index];
            nextSymbol();
        }
        if (Helper::isOctalLiteral(code[index - 1]))
        {
            state = false;
            return res;
        }
        state = true;
        return res;
    }

    while (Helper::isDigit(code[index]) && !Helper::isEof(code[index]))
    {
        res += code[index];
        nextSymbol();
    }

    state = true;
    return res;
}

std::string Tokenizer::parseKeyword()
{
    size_t oldIndex = index;
    if (!Helper::isValidIdentifierStart(code[index]))
    {
        state = false;
        return "";
    }

    std::string res;

    while (Helper::isValidIdentifierLetter(code[index]))
    {
        res += code[index];
        nextSymbol();
    }

    if (keywordTable.find(res) != keywordTable.end())
    {
        state = true;
        return res;
    }
    state = false;
    index = oldIndex;
    return "";
}

std::string Tokenizer::parseIdentifier()
{
    if (!Helper::isValidIdentifierStart(code[index]))
    {
        state = false;
        return "";
    }

    std::string res;

    while (Helper::isValidIdentifierLetter(code[index]) && !Helper::isEof(code[index]))
    {
        res += code[index];
        nextSymbol();
    }

    state = true;
    return res;
}

Token Tokenizer::nextToken()
{

    std::string whitespace = parseWhitespace();
    if (state)
        return Token{TokenType::Whitespace, whitespace};

    std::string comment = parseComment();
    if (state)
        return Token{TokenType::Comment, comment};

    switch (code[index])
    {
    case '+':
        nextSymbol();
        if (match('='))
            return Token{TokenType::PlusEqual, "+="};
        else if (match('+'))
            return Token{TokenType::PlusPlus, "++"};
        return Token{TokenType::Plus, "+"};

    case '-':
        nextSymbol();
        if (match('='))
            return Token{TokenType::MinusEqual, "-="};
        else if (match('+'))
            return Token{TokenType::MinusMinus, "--"};
        return Token{TokenType::Minus, "-"};

    case '*':
        nextSymbol();
        return match('=') ? Token{TokenType::MultiplyEqual, "*="} : Token{TokenType::Multiply, "*"};
    case '/':
        nextSymbol();
        return match('=') ? Token{TokenType::DivideEqual, "/="} : Token{TokenType::Divide, "/"};
    case '%':
        nextSymbol();
        return match('=') ? Token{TokenType::ModuloEqual, "%="} : Token{TokenType::Modulo, "%"};
    case '&':
        nextSymbol();
        if (match('^'))
            return Token{TokenType::BitAndNot, "&^"};
        else if (match('&'))
            return Token{TokenType::And, "&&"};
        return Token{TokenType::BitLogicalAnd, "&"};
    case '|':
        nextSymbol();
        return match('|') ? Token{TokenType::Or, "||"} : Token{TokenType::BitLogicalOr, "|"};
    case '^':
        nextSymbol();
        return Token{TokenType::BitLogicalXor, "^"};
    case '<':
        nextSymbol();
        if (match('<'))
            return Token{TokenType::LeftShift, "<<"};
        else if (match('-'))
            return Token{TokenType::Receive, "<-"};
        else if (match('='))
            return Token{TokenType::SmallerEquals, "<="};
        return Token{TokenType::Smaller, "<"};
    case '>':
        nextSymbol();
        if (match('>'))
            return Token{TokenType::RightShift, ">>"};
        else if (match('='))
            return Token{TokenType::GreaterEquals, ">="};
        return Token{TokenType::Greater, ">"};
    case '=':
        nextSymbol();
        return match('=') ? Token{TokenType::Equals, "=="} : Token{TokenType::Assignent, "="};
    case ':':
        nextSymbol();
        return match('=') ? Token{TokenType::Declare, ":="} : Token{TokenType::Colon, ":"};
    case ';':
        nextSymbol();
        return Token{TokenType::Semicolon, ";"};
    case '!':
        nextSymbol();
        return match('=') ? Token{TokenType::NotEqual, "!="} : Token{TokenType::Not, "!"};
    case '.':
        nextSymbol();
        if (match('.'))
            return match('.') ? Token{TokenType::ThreeDots, "..."} : Token{TokenType::Invalid, "..?"};
        return Token{TokenType::Dot, "."};
    case ',':
        nextSymbol();
        return Token{TokenType::Comma, ","};
    case '(':
        nextSymbol();
        return Token{TokenType::LeftBrace, "("};
    case ')':
        nextSymbol();
        return Token{TokenType::RightBrace, ")"};
    case '[':
        nextSymbol();
        return Token{TokenType::LeftSquareBrace, "["};
    case ']':
        nextSymbol();
        return Token{TokenType::RightSquareBrace, "]"};
    case '{':
        nextSymbol();
        return Token{TokenType::LeftCurlyBrace, "{"};
    case '}':
        nextSymbol();
        return Token{TokenType::RightCurlyBrace, "}"};

    default:
        break;
    }

    std::string strLiteral = parseString();
    if (state)
        return Token{TokenType::StringLit, strLiteral};

    std::string number = parseNumber();
    if (state)
        return Token{TokenType::Number, number};
    if (number != "")
        return Token{TokenType::Invalid, number};

    std::string keyword = parseKeyword();
    if (state)
        return Token{keywordTable[keyword], keyword};

    std::string identifier = parseIdentifier();
    if (state)
        return Token{TokenType::Identifier, identifier};

    nextSymbol();
    return Token{TokenType::Invalid, "?"};
}

LexerResult Tokenizer::tokenize()
{
    LexerResult res;

    do
    {
        Token token = nextToken();

        res.tokens.push_back(token);

    } while (!Helper::isEof(code[index]));

    return res;
}

void GenerateHTML::generate(LexerResult &lr, const std::string &filename)
{
    std::ofstream file(filename);

    file << "<!DOCTYPE html>\n\t<html>\n\t\t<head>\n\t\t\t<title>go2html</title>\n\t\t\t<style>\n.spelling-error {text-decoration-line: underline;text-decoration-style: wavy;text-decoration-color: red; color: white;}\n\t\t\t\t.keyword {\n\t\t\t\t\tcolor: #569CD6;\n\t\t\t\t}\n\t\t\t\t.number {\n\t\t\t\t\tcolor: #b5cea8;\n\t\t\t\t}\n\t\t\t\t.default {\n\t\t\t\t\tcolor: rgb(223, 235, 231);\n\t\t\t\t}\n\t\t\t\t.string {\n\t\t\t\t\tcolor: #CE9178;\n\t\t\t\t}\n\t\t\t\t.operator {\n\t\t\t\t\tfont-style: bold; color: #D4D4D4\n\t\t\t\t}\n\t\t\t\t.comment {\n\t\t\t\t\tcolor: #6A9955;\n\t\t\t\t}\n\t\t\t\t.identifier {\n\t\t\t\t\tcolor: #9CDCFE;\n\t\t\t\t}\n\t\t\t</style>\n\t\t</head>\n\t\t<body style=\"background-color:#1e1e1e\">\n\t\t\t<pre class=\"code\">";

    std::string text;

    for (Token &token : lr.tokens)
    {
        switch (token.type)
        {
        case TokenType::Comment:
            text = "<span class=" + std::string("\"comment\">") + token.lexeme + std::string("</span>");
            break;
        case TokenType::Identifier:
            text = "<span class=" + std::string("\"identifier\">") + token.lexeme + std::string("</span>");
            break;
        case TokenType::Number:
            text = "<span class=" + std::string("\"number\">") + token.lexeme + std::string("</span>");
            break;
        case TokenType::StringLit:
            text = "<span class=" + std::string("\"string\">") + token.lexeme + std::string("</span>");
            break;
        case TokenType::Invalid:
            text = "<span class=" + std::string("\"spelling-error\">") + token.lexeme + std::string("</span>");
            break;
        default:
            text = "<span class=" + std::string("\"default\">") + token.lexeme + std::string("</span>");
            break;
        }
        if ((unsigned)token.type >= 47 && (unsigned)token.type <= 78)
            text = "<span class=" + std::string("\"keyword\">") + token.lexeme + std::string("</span>");
        file << text;
    }

    file << "</pre></body></html>";
    file.close();
}