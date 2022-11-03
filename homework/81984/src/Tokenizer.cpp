#include "Tokenizer.hpp"

namespace Rb {
  #define KEYWORD_REGEX               Regex("^[a-z\\?]+")
  #define IDENTIFIER_REGEX            Regex("^([a-zA-Z_]+)([a-zA-Z0-9_\\?]*)")
  #define FUNCTION_IDENTIFIER_REGEX   Regex("^([a-zA-Z_]+)([a-zA-Z0-9_\\?]*)\\(")
  #define SYMBOL_REGEX                Regex("^:(([a-zA-Z_]+)([a-zA-Z0-9_]*)|'.*'|\".*\")")
  #define SINGLE_QUOTED_STRING_REGEX  Regex("^'.*'")
  #define DOUBLE_QUOTED_STRING_REGEX  Regex("^\".*\"")
  #define FUNCTION_IDENTIFIER_REGEX   Regex("^([a-zA-Z_]+)([a-zA-Z0-9_\\?]*)\\(")
  #define NUMBER_REGEX                Regex("^[+-]?([0-9]*[.])?[0-9]+")
  #define TRIPLE_DOT_REGEX            Regex("^\\.\\.\\.")
  #define DOUBLE_DOT_REGEX            Regex("^\\.\\.")
  #define TRIPLE_EQUAL_REGEX          Regex("^\\=\\=\\=")
  #define DOUBLE_EQUAL_REGEX          Regex("^\\=\\=")
  #define GREATER_THAN_OR_EQAUL_REGEX Regex("^>\\=")
  #define LESS_THAN_OR_EQAUL_REGEX    Regex("^<\\=")
  #define NO_REGEX_MATCH              String("")

  Tokenizer::Tokenizer() {
    Line = 0;
    Column = 0;
    Options = { .TokenizeWhitespaceCharacters = false, .StoreAllCharacterData = false };

    Keywords["alias"]    = Alias;
    Keywords["and"]      = And;
    Keywords["begin"]    = Begin;
    Keywords["break"]    = Break;
    Keywords["case"]     = Case;
    Keywords["class"]    = Class;
    Keywords["def"]      = Def;
    Keywords["defined?"] = Defined;
    Keywords["do"]       = Do;
    Keywords["else"]     = Else;
    Keywords["elsif"]    = Elsif;
    Keywords["end"]      = End;
    Keywords["ensure"]   = Ensure;
    Keywords["false"]    = False;
    Keywords["for"]      = For;
    Keywords["if"]       = If;
    Keywords["in"]       = In;
    Keywords["module"]   = Module;
    Keywords["next"]     = Next;
    Keywords["nil"]      = Nil;
    Keywords["not"]      = Not;
    Keywords["or"]       = Or;
    Keywords["redo"]     = Redo;
    Keywords["rescue"]   = Rescue;
    Keywords["retry"]    = Retry;
    Keywords["return"]   = Return;
    Keywords["self"]     = Self;
    Keywords["super"]    = Super;
    Keywords["then"]     = Then;
    Keywords["true"]     = True;
    Keywords["undef"]    = Undef;
    Keywords["unless"]   = Unless;
    Keywords["when"]     = When;
    Keywords["while"]    = While;
    Keywords["yield"]    = Yield;
  }

  Tokenizer::Tokenizer(TokenizerOptions TokenizerOptions) : Tokenizer() {
    Options = TokenizerOptions;
  }

  TokenList Tokenizer::TokenizeExpression(CStringReference Expression) {
    Line = 0;
    Column = 0;

    TokenList Tokens;

    while (*Expression != '\0') Tokens.push_back(ReadNextToken(Expression));

    return Tokens;
  }

  Token Tokenizer::ReadNextToken(CStringReference Expression) {
    Token Token = { .Type = Unknown, .Line = Line, .Column = Column };

    ReadWhitespace(Expression, Token);
    if (Token.Type != Unknown) return Token;

    ReadSpecialCharacter(Expression, Token);
    if (Token.Type != Unknown) return Token;

    ReadKeyword(Expression, Token);
    if (Token.Type != Unknown) return Token;

    ReadIdentifier(Expression, Token);
    if (Token.Type != Unknown) return Token;

    ReadSymbol(Expression, Token);
    if (Token.Type != Unknown) return Token;

    ReadString(Expression, Token);
    if (Token.Type != Unknown) return Token;

    ReadNumber(Expression, Token);
    if (Token.Type != Unknown) return Token;

    if (Options.StoreAllCharacterData) {
      Token.Data = *Expression;
    }

    Expression++;
    Column++;

    return Token;
  }

  void Tokenizer::ReadWhitespace(CStringReference Expression, TokenReference Token) {
    if (Options.TokenizeWhitespaceCharacters) {
      if (*Expression == ' ') {
        Token.Type = Space;

        if (Options.StoreAllCharacterData) {
          Token.Data = *Expression;
        }

        Expression++;
        Column++;
      } else if (*Expression == '\n') {
        Token.Type = NewLine;

        if (Options.StoreAllCharacterData) {
          Token.Data = *Expression;
        }

        Expression++;
        Column = 0;
        Line++;
      }
    } else {
      ClearWhitespaces(Expression);
    }
  }

  void Tokenizer::ReadSpecialCharacter(CStringReference Expression, TokenReference Token) {
    String MatchedSymbol = NO_REGEX_MATCH;

    switch (*Expression)
    {
      case '^': Token.Type = Caret;               break;
      case ',': Token.Type = Comma;               break;
      case '#': Token.Type = Octothorp;           break;
      case '}': Token.Type = ClosedCurlyBrace;    break;
      case ')': Token.Type = ClosedParenthesis;   break;
      case ']': Token.Type = ClosedSquareBracket; break;
      case '-': Token.Type = Dash;                break;
      case '.': Token.Type = ((MatchedSymbol = MatchRegex(Expression, TRIPLE_DOT_REGEX)) != NO_REGEX_MATCH) ? TripleDot : (MatchedSymbol = MatchRegex(Expression, DOUBLE_DOT_REGEX)) != NO_REGEX_MATCH ? DoubleDot : Dot;           break;
      case '=': Token.Type = ((MatchedSymbol = MatchRegex(Expression, TRIPLE_EQUAL_REGEX)) != NO_REGEX_MATCH) ? TripleEqual : (MatchedSymbol = MatchRegex(Expression, DOUBLE_EQUAL_REGEX)) != NO_REGEX_MATCH ? DoubleEqual : Equal; break;
      case '!': Token.Type = Exclamation;         break;
      case '>': Token.Type = ((MatchedSymbol = MatchRegex(Expression, GREATER_THAN_OR_EQAUL_REGEX)) != NO_REGEX_MATCH) ? GreaterThanOrEqual : GreaterThan; break;
      case '<': Token.Type = ((MatchedSymbol = MatchRegex(Expression, LESS_THAN_OR_EQAUL_REGEX)) != NO_REGEX_MATCH) ? LessThanOrEqual : LessThan;          break;
      case '{': Token.Type = OpenCurlyBrace;      break;
      case '(': Token.Type = OpenParenthesis;     break;
      case '[': Token.Type = OpenSquareBracket;   break;
      case '+': Token.Type = Plus;                break;
      case '/': Token.Type = Slash;               break;
      case '*': Token.Type = Star;                break;
    }

    if (Token.Type != Unknown) {
      if (MatchedSymbol != NO_REGEX_MATCH) {
        if (Options.StoreAllCharacterData) {
          Token.Data = MatchedSymbol;
        }

        Expression+= MatchedSymbol.length();
        Column += MatchedSymbol.length();
      } else {
        if (Options.StoreAllCharacterData) {
          Token.Data = *Expression;
        }

        Expression++;
        Column++;
      }
    }
  }

  void Tokenizer::ReadKeyword(CStringReference Expression, TokenReference Token) {
    String MatchedString = MatchRegex(Expression, KEYWORD_REGEX);

    if (Keywords.find(MatchedString) != Keywords.end()) {
      Token.Type = Keywords[MatchedString];

      if (Options.StoreAllCharacterData) {
        Token.Data = MatchedString;
      }

      Expression += MatchedString.length();
      Column += MatchedString.length();
    }
  }

  void Tokenizer::ReadIdentifier(CStringReference Expression, TokenReference Token) {
    String MatchedString = MatchRegex(Expression, IDENTIFIER_REGEX);

    if (MatchedString != NO_REGEX_MATCH) {
      Token.Type = Identifier;
      Token.Data = MatchedString;

      Expression += MatchedString.length();
      Column += MatchedString.length();
    }
  }

  void Tokenizer::ReadSymbol(CStringReference Expression, TokenReference Token) {
    String MatchedString = MatchRegex(Expression, SYMBOL_REGEX);

    if (MatchedString != NO_REGEX_MATCH) {
      Token.Type = Symbol;
      Token.Data = MatchedString;

      Expression += MatchedString.length();
      Column += MatchedString.length();
    }
  }

  void Tokenizer::ReadString(CStringReference Expression, TokenReference Token) {
    String MatchedString = MatchRegex(Expression, SINGLE_QUOTED_STRING_REGEX);

    if (MatchedString != NO_REGEX_MATCH) {
      Token.Type = SingleQuotedString;
      Token.Data = MatchedString;

      Expression += MatchedString.length();
      Column += MatchedString.length();
    } else {
      MatchedString = MatchRegex(Expression, DOUBLE_QUOTED_STRING_REGEX);

      if (MatchedString != NO_REGEX_MATCH) {
        Token.Type = SingleQuotedString;
        Token.Data = MatchedString;

        Expression += MatchedString.length();
        Column += MatchedString.length();
      }
    }
  }

  void Tokenizer::ReadNumber(CStringReference Expression, TokenReference Token) {
    String MatchedString = MatchRegex(Expression, NUMBER_REGEX);

    if (MatchedString != NO_REGEX_MATCH) {
      Token.Type = Number;
      Token.Data = MatchedString;

      Expression += MatchedString.length();
      Column += MatchedString.length();
    }
  }

  void Tokenizer::ClearWhitespaces(CStringReference Expression) {
    while (*Expression == ' ' || *Expression == '\n') {
      if (*Expression == '\n') {
        Column = 0;
        Line++;
      } else {
        Column++;
      }

      Expression++;
    }
  }

  String Tokenizer::MatchRegex(CStringReference Expression, Regex Regex) {
    RegexMatch Match;
    return std::regex_search(Expression, Match, Regex) ? Match[0] : NO_REGEX_MATCH;
  }
}
