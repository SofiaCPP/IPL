using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    class Lexer
    {
        private Dictionary<string, TokenType> idsMap;

        public Lexer()
        {
            idsMap = IdTokenTypes.AsMap();
        }

        public virtual List<Token> Tokenize()
        {
            return new List<Token>();
        }

    }
}
