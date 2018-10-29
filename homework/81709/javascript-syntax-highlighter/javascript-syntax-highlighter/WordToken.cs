using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    public class WordToken : Token, IEquatable<WordToken>
    {
        public WordToken(string lexeme, TokenType type) : base(type)
        {
            Lexeme = lexeme;
        }

        public string Lexeme { get; private set; }

        public override bool Equals(object obj)
        {
            var item = obj as WordToken;

            if(item == null)
            {
                return false;
            }

            return Equals(item);
        }

        public bool Equals(WordToken token)
        {
            return base.Equals((Token) token) && string.Equals(Lexeme, token.Lexeme);
        }

        public override int GetHashCode()
        {
            return Lexeme.GetHashCode() * 17 + base.GetHashCode();
        }

        public override string ToString()
        {
            return Lexeme;
        }
    }
}
