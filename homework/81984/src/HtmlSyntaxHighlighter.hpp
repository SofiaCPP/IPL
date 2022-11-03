#ifndef __HTML_SYNTAX_HIGHLIGHTER_HPP__
#define __HTML_SYNTAX_HIGHLIGHTER_HPP__

#include "Rb.hpp"
#include "Tokenizer.hpp"

namespace Rb {
  enum TokenColor {
    Blue,
    Yellow,
    Green,
    Magenta,
    Black,
    Red
  };

  struct HtmlSyntaxHighlighterOptions {
    bool ShowTokenInformationOnHover;
  };

  typedef ::std::unordered_map<TokenType, TokenColor> TokenColorMap;
  typedef TokenList& TokenListReference;

  class HtmlSyntaxHighlighter {
    TokenColorMap TokenColors;
    HtmlSyntaxHighlighterOptions Options;
  public:
    HtmlSyntaxHighlighter();
    HtmlSyntaxHighlighter(HtmlSyntaxHighlighterOptions HtmlViewOptions);

    void Highlight(String FileName);
  private:
    String ProcessTokens(TokenListReference Tokens);
    String ProcessTag(TokenReference Token);
    String ProcessTagType(TokenReference Token);
    String ProcessTagData(TokenReference Token);
    String ProcessAttributes(TokenReference Token);
    String ProcessTokenType(TokenReference Token);
    String ProcessTokenColor(TokenReference Token);
    String ProcessTokenData(TokenReference Token);

    String TokenInformationHoverScript();
  };
}

#endif
