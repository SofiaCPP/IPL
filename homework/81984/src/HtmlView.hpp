#ifndef __HTML_VIEW_HPP__
#define __HTML_VIEW_HPP__

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

  struct HtmlViewOptions {
    bool ShowTokenInformationOnHover;
  };

  typedef ::std::unordered_map<TokenType, TokenColor> TokenColorMap;
  typedef TokenList& TokenListReference;

  class HtmlView {
    TokenColorMap TokenColors;
    HtmlViewOptions Options;
  public:
    HtmlView();
    HtmlView(HtmlViewOptions HtmlViewOptions);

    void Visualize(String FileName);
  private:
    String ProcessTokens(TokenListReference Tokens);
    String ProcessTag(TokenReference Token);
    String ProcessAttributes(TokenReference Token);
    String ProcessTokenType(TokenReference Token);
    String ProcessTokenColor(TokenReference Token);
    String ProcessTokenData(TokenReference Token);

    String TokenInformationHoverScript();
  };
}

#endif
