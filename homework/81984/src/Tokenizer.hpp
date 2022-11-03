#ifndef __TOKENIZER_HPP__
#define __TOKENIZER_HPP__

#include "Rb.hpp"

namespace Rb {
  enum TokenType {
    Identifier,
    Symbol,
    SingleQuotedString,
    DoubleQuotedString,
    Number,

    Alias,
    And,
    Begin,
    Break,
    Case,
    Class,
    Def,
    Defined,
    Do,
    Else,
    Elsif,
    End,
    Ensure,
    False,
    For,
    If,
    In,
    Module,
    Next,
    Nil,
    Not,
    Or,
    Redo,
    Rescue,
    Retry,
    Return,
    Self,
    Super,
    Then,
    True,
    Undef,
    Unless,
    When,
    While,
    Yield,

    Caret,
    Comma,
    ClosedCurlyBrace,
    ClosedParenthesis,
    ClosedSquareBracket,
    Dash,
    Dot,
    DoubleDot,
    DoubleEqual,
    Equal,
    Exclamation,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Octothorp,
    OpenCurlyBrace,
    OpenParenthesis,
    OpenSquareBracket,
    Plus,
    Slash,
    Star,
    TripleDot,
    TripleEqual,

    NewLine,
    Space,

    Unknown
  };

  struct Token {
    TokenType Type;
    String Data;
    Index Line;
    Index Column;
  };

  struct TokenizerOptions {
    bool TokenizeWhitespaceCharacters;
    bool StoreAllCharacterData;
  };

  typedef ::std::unordered_map<std::string, TokenType> KeywordMap;
  typedef ::std::vector<Token> TokenList;
  typedef Token& TokenReference;

  class Tokenizer {
    Index Line;
    Index Column;
    Token PreviousToken;
    KeywordMap Keywords;
    TokenizerOptions Options;
  public:
    Tokenizer();
    Tokenizer(TokenizerOptions TokenizerOptions);

    TokenList TokenizeExpression(CStringReference Expression);
  private:
    Token ReadNextToken(CStringReference Expression);

    void ReadWhitespace(CStringReference Expression, TokenReference Token);
    void ReadSpecialCharacter(CStringReference Expression, TokenReference Token);
    void ReadKeyword(CStringReference Expression, TokenReference Token);
    void ReadIdentifier(CStringReference Expression, TokenReference Token);
    void ReadSymbol(CStringReference Expression, TokenReference Token);
    void ReadString(CStringReference Expression, TokenReference Token);
    void ReadNumber(CStringReference Expression, TokenReference Token);

    void ClearWhitespaces(CStringReference Expression);
    String MatchRegex(CStringReference Expression, Regex Regex);
  };
}

#endif
