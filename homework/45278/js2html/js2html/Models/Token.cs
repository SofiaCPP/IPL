using System;
using System.Collections.Generic;
using System.Text;

namespace js2html.Models
{
    public class Token
    {
        public TokenType TokenType { get; set; }
        public int Line { get; set; }
        public string Lexeme { get; set; }
        public double NumericValue { get; set; }

        public Token(TokenType tokenType, int line, string lexeme = "", double numericValue = 0 )
        {
            TokenType = tokenType;
            Line = line;
            Lexeme = lexeme;
            NumericValue = numericValue;
        }
    }
}
