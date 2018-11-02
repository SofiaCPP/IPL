using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace javascript_syntax_highlighter
{
    public static class SyntaxHighlighter
    {
        private static readonly string HTML =
            @"<!DOCTYPE html>
<html>
<head>
{0}
<title> {1} </title>
</head>
<body> {2} </body></html>";

        private static readonly string DEFAULT_CSS =
            @"<style>
body {
  font-size: 20px;
  font-family: ""Verdana"";
}
.number {
  color: Orange;
}
.string {
  color: Gray;
}
.keyword {
  color: Blue;
}
.identifier {
  color: Black;
}
.operator {
  color: DarkBlue;
}
.comment {
  color: Green;
  font-style: italic;
  white-space: pre-wrap;
}

.reserved {
  color: MediumVioletRed;
}

.undefined{
  color: Black;
}

.default {
  color: DarkBlue;
}
</style>";

        private static IEnumerable<TokenType> KEYWORDS = ReservedStrings.KeywordsAsMap().Values;

        private static IEnumerable<TokenType> OTHER_RESERVED = ReservedStrings.OthersAsMap().Values;
        
        private static IEnumerable<TokenType> ONE_SYMBOL_OPERATORS = new List<TokenType>
        {
            TokenType.Minus, // -
            TokenType.Plus, // +
            TokenType.Star, // *
            TokenType.Division, // /
            TokenType.Modulo, // %
            TokenType.BitwiseNot, // ~
            TokenType.BitwiseAnd, // &
            TokenType.BitwiseXor, // ^
            TokenType.BitwiseOr, // |
            TokenType.QuestionMark, // ?
            TokenType.Bang, // !
            TokenType.Equal, // =
            TokenType.Greater, // >
            TokenType.Less, // <
            TokenType.Backslash, // \           
        };

        private static IEnumerable<TokenType> MULT_SYMBOL_OPERATORS = new List<TokenType>
        {
            TokenType.NotEqual, // !=
            TokenType.EqualEqual, // ==
            TokenType.StrictEqual, // ===
            TokenType.StrictNotEqual, // !==
            TokenType.GreaterEqual, // >=
            TokenType.LessEqual, // <=
            TokenType.MinusMinus, // --
            TokenType.PlusPlus, // ++
            TokenType.LeftShift, // <<
            TokenType.RightShift, // >>
            TokenType.LogicalAnd, // &&
            TokenType.LogicalOr, // ||
            TokenType.StarEqual, // *=
            TokenType.DivideEqual, // /=
            TokenType.ModuloEqual, // %=
            TokenType.PlusEqual, // +=
            TokenType.MinusEqual, // -=
            TokenType.LeftShiftEqual, // <<=
            TokenType.RightShiftEqual, // >>=
            TokenType.BitwiseAndEqual, // &=
            TokenType.BitwiseXorEqual, // ^=
            TokenType.BitwiseOrEqual, // |=         
        };

        public static void GenerateHtml(IEnumerable<Token> tokens, string fileName, string css = null)
        {
            if (css == null) css = DEFAULT_CSS;

            string code = "";
            IEnumerator<Token> tokenEnum = tokens.GetEnumerator();
            if (!tokenEnum.MoveNext()) return;
            Token cur;
            do
            {
                cur = tokenEnum.Current;
                TokenType type = cur.Type;

                switch (type)
                {
                    case TokenType.Whitespace:
                        code += "&nbsp";
                        break;
                    case TokenType.Tab:
                        code += "&nbsp" + "&nbsp" + "&nbsp" + "&nbsp";
                        break;
                    case TokenType.NewLine:
                        code += "<br>";
                        break;
                    case TokenType.Number:
                        code += "<span class=\"number\">" + ((NumToken)cur).Num + "</span>" + Environment.NewLine;
                        break;
                    case TokenType.String:
                        code += "<span class=\"string\">" + ((WordToken)cur).Lexeme + "</span>" + Environment.NewLine;
                        break;
                    case TokenType.Identifier:
                        code += "<span class=\"identifier\">" + ((WordToken)cur).Lexeme + "</span>" + Environment.NewLine;
                        break;
                    case TokenType.Comment:
                        code += "<span class=\"comment\">" + ((WordToken)cur).Lexeme + "</span>" + Environment.NewLine;
                        break;
                    case TokenType.Error:
                        code += "<span class=\"undefined\">" + ((WordToken)cur).Lexeme + "</span>" + Environment.NewLine;
                        break;

                    default:
                        if (KEYWORDS.Contains(type))
                        {
                            code += "<span class=\"keyword\">" + ((WordToken)cur).Lexeme + "</span>" + Environment.NewLine;
                        }
                        else if(OTHER_RESERVED.Contains(type))
                        {
                            code += "<span class=\"reserved\">" + ((WordToken)cur).Lexeme + "</span>" + Environment.NewLine;
                        }
                        else if (ONE_SYMBOL_OPERATORS.Contains(type))
                        {
                            code += "<span class=\"operator\">" + cur.ToString() + "</span>" + Environment.NewLine;
                        }
                        else if (MULT_SYMBOL_OPERATORS.Contains(type))
                        {
                            code += "<span class=\"operator\">" + ((WordToken)cur).Lexeme + "</span>" + Environment.NewLine;
                        }
                        else
                        {
                            code += "<span class=\"default\">" + cur.ToString() + "</span>" + Environment.NewLine;
                            break;
                        }
                        break;
                }
            }
            while (tokenEnum.MoveNext() && cur.Type != TokenType.EOF);

            string shouldWrite = string.Format(HTML, css, fileName, code);

            using (StreamWriter writer = new StreamWriter(fileName))
            {
                writer.Write(shouldWrite);
            }
        }
    }
}
