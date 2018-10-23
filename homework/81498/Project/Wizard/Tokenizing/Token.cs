using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard
{
    public class Token
    {
        public Token(TokenType type, int line, string lexeme, string location, decimal? number)
        {
            Type = type;
            Line = line;
            Lexeme = lexeme;
            Location = location;
            Number = number;

        }

        public TokenType Type { get; private set; }
        public int Line { get; private set; }
        public string Lexeme { get; private set; }
        public decimal? Number { get; private set; }
        public string Location { get; private set; }

        public override string ToString()
        {
            return Type.ToString() + " " + Lexeme + " " + Number;
        }

    }
}
