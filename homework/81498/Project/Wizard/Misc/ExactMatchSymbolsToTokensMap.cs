using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard.Misc
{   // is this a good idea?
    public static class ExactMatchSymbolsToTokens
    {
        public static Dictionary<char, TokenType> AsMap()
        {
            return new Dictionary<char, TokenType>()
            {
                {' ', TokenType.Whitespace },
                {'\t', TokenType.Tab },
                {':', TokenType.Colon },
                {',', TokenType.Comma },
                {'(', TokenType.LeftParenthesis },
                {'{', TokenType.LeftBrace },
                {'[', TokenType.LeftSquareBracket },
                {']', TokenType.RightSquareBracket },
                {'}', TokenType.RightBrace },
                {')', TokenType.RightParenthesis },
                {';', TokenType.Semicolon },
                {'\'', TokenType.SingleQuote },
                {'?', TokenType.SingleQuote },
                {'.', TokenType.SingleQuote },
            };
        }
    }
}
