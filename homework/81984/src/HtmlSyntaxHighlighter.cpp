#include "HtmlSyntaxHighlighter.hpp"
#include "FileProcessor.hpp"
#include "Tokenizer.hpp"

#define VIEW_WRAPPER_BEGIN "<div style=\""             \
                           " display: flex;"           \
                           " width: 100%;"             \
                           " height: 100%;"            \
                           " justify-content: center;" \
                           " align-items: center;"     \
                           " font-size: 3.5rem;"       \
                           "\"><div style=\""          \
                           " width: 100%;"             \
                           " height: 100%;"            \
                           "\">"

#define VIEW_WRAPPER_END "</div></div>"

namespace Rb {
  HtmlSyntaxHighlighter::HtmlSyntaxHighlighter() {
    Options = { .ShowTokenInformationOnHover = false };

    TokenColors[Identifier]          = Blue;
    TokenColors[Symbol]              = Blue;
    TokenColors[SingleQuotedString]  = Red;
    TokenColors[DoubleQuotedString ] = Red;
    TokenColors[Number]              = Green;

    TokenColors[Class]   = Magenta;
    TokenColors[Def]     = Magenta;
    TokenColors[Do]      = Magenta;
    TokenColors[Else]    = Magenta;
    TokenColors[Elsif]   = Magenta;
    TokenColors[End]     = Magenta;
    TokenColors[False]   = Magenta;
    TokenColors[For]     = Magenta;
    TokenColors[If]      = Magenta;
    TokenColors[Module]  = Magenta;
    TokenColors[Nil]     = Magenta;
    TokenColors[Return]  = Magenta;
    TokenColors[Self]    = Magenta;
    TokenColors[Super]   = Magenta;
    TokenColors[True]    = Magenta;
    TokenColors[Unless]  = Magenta;
    TokenColors[While]   = Magenta;
    TokenColors[Yield]   = Magenta;

    TokenColors[Comma]               = Black;
    TokenColors[ClosedCurlyBrace]    = Black;
    TokenColors[ClosedParenthesis]   = Black;
    TokenColors[ClosedSquareBracket] = Black;
    TokenColors[Dash]                = Black;
    TokenColors[Dot]                 = Black;
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
  }

  HtmlSyntaxHighlighter::HtmlSyntaxHighlighter(HtmlSyntaxHighlighterOptions HtmlSyntaxHighlighterOptions) : HtmlSyntaxHighlighter() {
    Options = HtmlSyntaxHighlighterOptions;
  }

  void HtmlSyntaxHighlighter::Highlight(String FileName) {
    String Expression = FileProcessor::ReadFile(FileName);
    CString ExpressionReference = Expression.c_str();

    TokenizerOptions TokenizerOptions = { .TokenizeWhitespaceCharacters = true, .StoreAllCharacterData = true };
    Tokenizer Tokenizer(TokenizerOptions);
    TokenList Tokens = Tokenizer.TokenizeExpression(ExpressionReference);

    String View = ProcessTokens(Tokens);

    if (Options.ShowTokenInformationOnHover) {
      View += TokenInformationHoverScript();
    }

    String ViewFileName = String(FileName) + ".html";

    FileProcessor::WriteFile(ViewFileName, View);
  }

  String HtmlSyntaxHighlighter::ProcessTokens(TokenListReference Tokens) {
    String View = VIEW_WRAPPER_BEGIN;

    for (Token Token : Tokens) View += ProcessTag(Token);

    View += VIEW_WRAPPER_END;

    return View;
  }

  String HtmlSyntaxHighlighter::ProcessTag(TokenReference Token) {
    String TagType = ProcessTagType(Token);
    String TagData = ProcessTagData(Token);

    String Tag = "<" + TagType + " " + ProcessAttributes(Token) + ">";
    if (TagData != "") Tag += TagData + "</" + TagType + ">";

    return Tag;
  }

  String HtmlSyntaxHighlighter::ProcessTagType(TokenReference Token) {
    switch (Token.Type) {
      case NewLine: return "br";
      default:      return "strong";
    }
  }

  String HtmlSyntaxHighlighter::ProcessTagData(TokenReference Token) {
    switch (Token.Type) {
      case Space:   return "&nbsp";
      case NewLine: return "";
      default:      return Token.Data;
    }
  }

  String HtmlSyntaxHighlighter::ProcessAttributes(TokenReference Token) {
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

  String HtmlSyntaxHighlighter::ProcessTokenColor(TokenReference Token) {
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

  String HtmlSyntaxHighlighter::ProcessTokenType(TokenReference Token) {
    switch (Token.Type)
    {
    case Identifier:          return "Identifier";
    case Symbol:              return "Symbol";
    case SingleQuotedString:  return "SingleQuotedString";
    case DoubleQuotedString:  return "DoubleQuotedString";
    case Number:              return "Number";
    case Class:               return "Class";
    case Def:                 return "Def";
    case Do:                  return "Do";
    case Else:                return "Else";
    case Elsif:               return "Elsif";
    case End:                 return "End";
    case False:               return "False";
    case For:                 return "For";
    case If:                  return "If";
    case Module:              return "Module";
    case Nil:                 return "Nil";
    case Return:              return "Return";
    case Self:                return "Self";
    case Super:               return "Super";
    case True:                return "True";
    case Unless:              return "Unless";
    case While:               return "While";
    case Yield:               return "Yield";
    case Comma:               return "Comma";
    case ClosedCurlyBrace:    return "ClosedCurlyBrace";
    case ClosedParenthesis:   return "ClosedParenthesis";
    case ClosedSquareBracket: return "ClosedSquareBracket";
    case Dash:                return "Dash";
    case Dot:                 return "Dot";
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
    case NewLine:             return "NewLine";
    case Space:               return "Space";
    default:                  return "Unknown";
    }
  }

  String HtmlSyntaxHighlighter::ProcessTokenData(TokenReference Token) {
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

  String HtmlSyntaxHighlighter::TokenInformationHoverScript() {
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
