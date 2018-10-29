using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    public class NumToken : Token, IEquatable<NumToken>
    {
        public NumToken(long num) : base(TokenType.Number)
        {
            Num = num;
        }

        public long Num { get; private set; }

        public bool Equals(NumToken other)
        {
            return other.Num == Num && base.Equals((Token)other);
        }

        public override bool Equals(object obj)
        {
            var item = obj as NumToken;

            if (item == null) return false;

            return Equals(item);
        }

        public override int GetHashCode()
        {
            return Num.GetHashCode() * 17 + base.GetHashCode();
        }

        public override string ToString()
        {
            return Num.ToString();
        }
    }
}
