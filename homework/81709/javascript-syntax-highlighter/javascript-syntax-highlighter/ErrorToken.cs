using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    class ErrorToken : Token
    {
        public ErrorToken(int line, string what) : base(TokenType.Invalid)
        {
            Line = line;
            What = what;
        }

        public int Line { get; private set; }

        public string What { get; private set; }

        public override bool Equals(object obj)
        {
            var item = obj as ErrorToken;

            if (item == null)
            {
                return false;
            }

            return Equals(item);
        }

        public bool Equals(ErrorToken token)
        {
            return base.Equals(token) 
                && string.Equals(What, token.What)
                && Line == token.Line;
        }

        public override int GetHashCode()
        {
            int hash = 13;
            hash = hash * 7 + What.GetHashCode();
            hash = hash * 7 + Line.GetHashCode();
            hash = hash * 7 + base.GetHashCode();
            return hash;
        }
    }
}
