using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard
{
    public class Token
    {
        public Token(TokenType type, uint line, string lexeme, string location, double? number)
        {
            Type = type;
            Line = line;
            Lexeme = lexeme;
            Location = location;
            Number = number;

        }

        public TokenType Type { get; private set; }
        public uint Line { get; private set; }
        public string Lexeme { get; private set; }
        public double? Number { get; private set; }
        public string Location { get; private set; }

        public override string ToString()
        {
            return Type.ToString() + " " + Lexeme + " " + Number;
        }

    }
}
