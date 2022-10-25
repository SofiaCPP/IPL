#include "HtmlView.hpp"
#include "FileProcessor.hpp"
#include "Tokenizer.hpp"

namespace Rb {
  HtmlView::HtmlView() {
    Options = { .ShowTokenInformationOnHover = false };

    TokenColors[Identifier]          = Blue;
    TokenColors[FunctionIdentifier]  = Yellow;
    TokenColors[Symbol]              = Blue;
    TokenColors[SingleQuotedString]  = Red;
    TokenColors[DoubleQuotedString ] = Red;
    TokenColors[Number]              = Green;

    TokenColors[Alias]   = Magenta;
    TokenColors[And]     = Magenta;
    TokenColors[Begin]   = Magenta;
    TokenColors[Break]   = Magenta;
    TokenColors[Case]    = Magenta;
    TokenColors[Class]   = Magenta;
    TokenColors[Def]     = Magenta;
    TokenColors[Defined] = Magenta;
    TokenColors[Do]      = Magenta;
    TokenColors[Else]    = Magenta;
    TokenColors[Elsif]   = Magenta;
    TokenColors[End]     = Magenta;
    TokenColors[Ensure]  = Magenta;
    TokenColors[False]   = Magenta;
    TokenColors[For]     = Magenta;
    TokenColors[If]      = Magenta;
    TokenColors[In]      = Magenta;
    TokenColors[Module]  = Magenta;
    TokenColors[Next]    = Magenta;
    TokenColors[Nil]     = Magenta;
    TokenColors[Not]     = Magenta;
    TokenColors[Or]      = Magenta;
    TokenColors[Redo]    = Magenta;
    TokenColors[Rescue]  = Magenta;
    TokenColors[Retry]   = Magenta;
    TokenColors[Return]  = Magenta;
    TokenColors[Self]    = Magenta;
    TokenColors[Super]   = Magenta;
    TokenColors[Then]    = Magenta;
    TokenColors[True]    = Magenta;
    TokenColors[Undef]   = Magenta;
    TokenColors[Unless]  = Magenta;
    TokenColors[When]    = Magenta;
    TokenColors[While]   = Magenta;
    TokenColors[Yield]   = Magenta;

    TokenColors[Caret]               = Black;
    TokenColors[Comma]               = Black;
    TokenColors[ClosedCurlyBrace]    = Black;
    TokenColors[ClosedParenthesis]   = Black;
    TokenColors[ClosedSquareBracket] = Black;
    TokenColors[Dash]                = Black;
    TokenColors[Dot]                 = Black;
    TokenColors[DoubleDot]           = Black;
    TokenColors[DoubleEqual]         = Black;
    TokenColors[Equal]               = Black;
    TokenColors[Exclamation]         = Black;
    TokenColors[GreaterThan]         = Black;
    TokenColors[GreaterThanOrEqual]  = Black;
    TokenColors[LessThan]            = Black;
    TokenColors[LessThanOrEqual]     = Black;
    TokenColors[Octothorp]           = Green;
    TokenColors[OpenCurlyBrace]      = Black;
    TokenColors[OpenParenthesis]     = Black;
    TokenColors[OpenSquareBracket]   = Black;
    TokenColors[Plus]                = Black;
    TokenColors[Slash]               = Black;
    TokenColors[Star]                = Black;
    TokenColors[TripleDot]           = Black;
    TokenColors[TripleEqual]         = Black;
  }

  HtmlView::HtmlView(HtmlViewOptions HtmlViewOptions) : HtmlView() {
    Options = HtmlViewOptions;
  }

  void HtmlView::Visualize(String FileName) {
    String Expression = FileProcessor::ReadFile(FileName);
    CString ExpressionReference = Expression.c_str();

    TokenizerOptions TokenizerOptions = { .TokenizeWhitespaceCharacters = true, .StoreAllCharacterData = true };
    Tokenizer Tokenizer(TokenizerOptions);
    TokenList Tokens = Tokenizer.TokenizeExpression(ExpressionReference);

    String View = ProcessTokens(Tokens);

    if (Options.ShowTokenInformationOnHover) {
      View += TokenInformationHoverScript();
    }

    String TargetFileName = String(FileName) + ".html";

    FileProcessor::WriteFile(TargetFileName, View);
  }

  String HtmlView::ProcessTokens(TokenListReference Tokens) {
    String Tags = "<div style=\""
                  " display: flex;"
                  " width: 100%;"
                  " height: 100%;"
                  " justify-content: center;"
                  " align-items: center;"
                  " font-size: 3.5rem;"
                  "\"><div style=\""
                  " width: 100%;"
                  " height: 100%;"
                  "\">";

    for (Token Token : Tokens) Tags += ProcessTag(Token);

    Tags += "</div></div>";

    return Tags;
  }

  String HtmlView::ProcessTag(TokenReference Token) {
    String TagType = "";
    String TagData = "";

    switch (Token.Type)
    {
    case NewLine:
      TagType = "br";
      break;
    case Space:
      TagType = "strong";
      TagData = "&nbsp;";
      break;
    default:
      TagType = "strong";
      TagData = Token.Data;
      break;
    }

    String Tag = "<" + TagType + " " + ProcessAttributes(Token) + ">";

    if (TagData != "") {
      Tag += TagData + "</" + TagType + ">";
    }

    return Tag;
  }

  String HtmlView::ProcessAttributes(TokenReference Token) {
    return "class=\"token\""
           "style=\"color: " + ProcessTokenColor(Token) + "\""
           "data-html=\"true\""
           "data-content=\""
              "Type: "   + ProcessTokenType(Token)        + "<br>"
              "Data: "   + ProcessTokenData(Token)        + "<br>"
              "Line: "   + ::std::to_string(Token.Line)   + "<br>"
              "Column: " + ::std::to_string(Token.Column) + "<br>"
            "\"";
  }

  String HtmlView::ProcessTokenColor(TokenReference Token) {
    switch (TokenColors[Token.Type])
    {
    case Blue:    return "#00148C";
    case Yellow:  return "#795E26";
    case Green:   return "#098658";
    case Magenta: return "#C90CDB";
    case Black:   return "#100421";
    case Red:     return "#C12015";
    default:      return "#FFFFFF";
    }
  }

  String HtmlView::ProcessTokenType(TokenReference Token) {
    switch (Token.Type)
    {
    case FunctionIdentifier:  return "FunctionIdentifier";
    case Identifier:          return "Identifier";
    case Symbol:              return "Symbol";
    case SingleQuotedString:  return "SingleQuotedString";
    case DoubleQuotedString:  return "DoubleQuotedString";
    case Number:              return "Number";
    case Alias:               return "Alias";
    case And:                 return "And";
    case Begin:               return "Begin";
    case Break:               return "Break";
    case Case:                return "Case";
    case Class:               return "Class";
    case Def:                 return "Def";
    case Defined:             return "Defined";
    case Do:                  return "Do";
    case Else:                return "Else";
    case Elsif:               return "Elsif";
    case End:                 return "End";
    case Ensure:              return "Ensure";
    case False:               return "False";
    case For:                 return "For";
    case If:                  return "If";
    case In:                  return "In";
    case Module:              return "Module";
    case Next:                return "Next";
    case Nil:                 return "Nil";
    case Not:                 return "Not";
    case Or:                  return "Or";
    case Redo:                return "Redo";
    case Rescue:              return "Rescue";
    case Retry:               return "Retry";
    case Return:              return "Return";
    case Self:                return "Self";
    case Super:               return "Super";
    case Then:                return "Then";
    case True:                return "True";
    case Undef:               return "Undef";
    case Unless:              return "Unless";
    case When:                return "When";
    case While:               return "While";
    case Yield:               return "Yield";
    case Caret:               return "Caret";
    case Comma:               return "Comma";
    case ClosedCurlyBrace:    return "ClosedCurlyBrace";
    case ClosedParenthesis:   return "ClosedParenthesis";
    case ClosedSquareBracket: return "ClosedSquareBracket";
    case Dash:                return "Dash";
    case Dot:                 return "Dot";
    case DoubleDot:           return "DoubleDot";
    case DoubleEqual:         return "DoubleEqual";
    case Equal:               return "Equal";
    case Exclamation:         return "Exclamation";
    case GreaterThan:         return "GreaterThan";
    case GreaterThanOrEqual:  return "GreaterThanOrEqual";
    case LessThan:            return "LessThan";
    case LessThanOrEqual:     return "LessThanOrEqual";
    case Octothorp:           return "Octothorp";
    case OpenCurlyBrace:      return "OpenCurlyBrace";
    case OpenParenthesis:     return "OpenParenthesis";
    case OpenSquareBracket:   return "OpenSquareBracket";
    case Plus:                return "Plus";
    case Slash:               return "Slash";
    case Star:                return "Star";
    case TripleDot:           return "TripleDot";
    case TripleEqual:         return "TripleEqual";
    case NewLine:             return "NewLine";
    case Space:               return "Space";
    default:                  return "Unknown";
    }
  }

  String HtmlView::ProcessTokenData(TokenReference Token) {
    String Data = "";

    for (char Character : Token.Data) {
      if (Character == '"') {
        Data += "&quot;";
      } else {
        Data += Character;
      }
    }

    return Data;
  }

  String HtmlView::TokenInformationHoverScript() {
    return "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css\">"
           "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js\"></script>"
           "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js\"></script>"
           "<script>"
           "  $(document).ready(() => {"
           "    $('.token').popover({"
           "      trigger: 'hover',"
           "      placement: 'right'"
           "    });"
           "  });"
           "</script>";
  }
}
