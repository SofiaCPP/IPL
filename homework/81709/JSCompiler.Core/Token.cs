using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JSCompiler.Core
{
    public class Token : IEquatable<Token>
    {
        public Token(TokenType type)
        {
            Type = type;
        }

        public TokenType Type { get; private set; }

        public override bool Equals(object obj)
        {
            var item = obj as Token;

            if(item == null)
            {
                return false;
            }

            return Equals(item);
        }

        public bool Equals(Token other)
        {
            return Type == other.Type;
        }

        public override int GetHashCode()
        {
            return this.Type.GetHashCode();
        }

        public override string ToString()
        {
            return ((char)Type).ToString();
        }
    }



}
