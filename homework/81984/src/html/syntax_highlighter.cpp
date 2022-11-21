#include "syntax_highlighter.hpp"
#include "../utils/file_processor.hpp"

#define VIEW_WRAPPER_BEGIN "<div style=\""             \
                           " display: flex;"           \
                           " width: 100%;"             \
                           " height: 100%;"            \
                           " justify-content: center;" \
                           " align-items: center;"     \
                           " font-size: 3.5rem;"       \
                           "\"><pre style=\""          \
                           " width: 100%;"             \
                           " height: 100%;"            \
                           "\">"

#define VIEW_WRAPPER_END "</pre></div>"

#define BOOTSTRAP_HOVER "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css\">" \
                        "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js\"></script>"               \
                        "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js\"></script>"            \
                        "<script>"                                                                                                 \
                        "  $(document).ready(() => {"                                                                              \
                        "    $('.token').popover({"                                                                                \
                        "      trigger: 'hover',"                                                                                  \
                        "      placement: 'right'"                                                                                 \
                        "    });"                                                                                                  \
                        "  });"                                                                                                    \
                        "</script>"

SyntaxHighlighter::SyntaxHighlighter(SyntaxHighlighterOptions options) : options_(options)
{
  pallete_[Identifier]         = Blue;
  pallete_[Symbol]             = Blue;
  pallete_[SingleQuotedString] = Red;
  pallete_[DoubleQuotedString] = Red;
  pallete_[Number]             = Green;

  pallete_[Class]   = Magenta;
  pallete_[Def]     = Magenta;
  pallete_[Do]      = Magenta;
  pallete_[Else]    = Magenta;
  pallete_[Elsif]   = Magenta;
  pallete_[End]     = Magenta;
  pallete_[If]      = Magenta;
  pallete_[Module]  = Magenta;
  pallete_[Unless]  = Magenta;
  pallete_[While]   = Magenta;

  pallete_[Comma]               = Black;
  pallete_[ClosedParenthesis]   = Black;
  pallete_[Dot]                 = Black;
  pallete_[DoubleEqual]         = Black;
  pallete_[Equal]               = Black;
  pallete_[GreaterThan]         = Black;
  pallete_[GreaterThanOrEqual]  = Black;
  pallete_[LessThan]            = Black;
  pallete_[LessThanOrEqual]     = Black;
  pallete_[Octothorp]           = Green;
  pallete_[OpenParenthesis]     = Black;
}

void SyntaxHighlighter::Highlight(String file_name)
{
  String expression = FileProcessor::ReadFile(file_name);
  CString cstring_expression = expression.c_str();

  LexerOptions lexer_options = { .tokenize_spaces = true, .tokenize_new_lines = true, .store_all_data = true };
  Lexer lexer(lexer_options);
  Vector<Token> tokens = lexer.TokenizeExpression(cstring_expression);

  String view = ProcessTokens(tokens);

  if (options_.hover_information) {
    view += BOOTSTRAP_HOVER;
  }

  String view_file_name = file_name + ".html";

  FileProcessor::WriteFile(view_file_name, view);
}

String SyntaxHighlighter::ProcessTokens(Vector<Token>& tokens) {
  String view = VIEW_WRAPPER_BEGIN;

  for (Token token : tokens) view += ProcessTag(token);

  view += VIEW_WRAPPER_END;

  return view;
}

String SyntaxHighlighter::ProcessTag(Token& token) {
  String tag_type = ProcessTagType(token);
  String tag_data = token.data;

  String tag = "<" + tag_type + " " + ProcessAttributes(token) + ">";
  if (tag_data != "") tag += tag_data + "</" + tag_type + ">";

  return tag;
}

String SyntaxHighlighter::ProcessTagType(Token& token) {
  switch (token.type) {
    case NewLine: return "br";
    default:      return "strong";
  }
}

String SyntaxHighlighter::ProcessAttributes(Token& token) {
  return "class=\"token\""
          "style=\"color: " + ProcessTokenColor(token) + "\""
          "data-html=\"true\""
          "data-content=\""
            "Type: "   + ProcessTokenType(token)        + "<br>"
            "Data: "   + ProcessTokenData(token)        + "<br>"
            "Line: "   + ::std::to_string(token.line)   + "<br>"
            "Column: " + ::std::to_string(token.column) + "<br>"
          "\"";
}

String SyntaxHighlighter::ProcessTokenColor(Token& token) {
  switch (pallete_[token.type])
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

String SyntaxHighlighter::ProcessTokenType(Token& token) {
  switch (token.type)
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
  case If:                  return "If";
  case Module:              return "Module";
  case Unless:              return "Unless";
  case While:               return "While";
  case Comma:               return "Comma";
  case ClosedParenthesis:   return "ClosedParenthesis";
  case Dot:                 return "Dot";
  case DoubleEqual:         return "DoubleEqual";
  case Equal:               return "Equal";
  case GreaterThan:         return "GreaterThan";
  case GreaterThanOrEqual:  return "GreaterThanOrEqual";
  case LessThan:            return "LessThan";
  case LessThanOrEqual:     return "LessThanOrEqual";
  case Octothorp:           return "Octothorp";
  case OpenParenthesis:     return "OpenParenthesis";
  case NewLine:             return "NewLine";
  case Space:               return "Space";
  default:                  return "Unknown";
  }
}

String SyntaxHighlighter::ProcessTokenData(Token& token) {
  String data = "";

  for (Char character : token.data) {
    if (character == '"') {
      data += "&quot;";
    } else {
      data += character;
    }
  }

  return data;
}
