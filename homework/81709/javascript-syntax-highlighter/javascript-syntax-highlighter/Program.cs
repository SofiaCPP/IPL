using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    class Program
    {
        static void Main(string[] args)
        {
            InputStream stream = new InputStream("test.js");
            Lexer lex = new Lexer(stream);
            List<Token> tokens = lex.Tokenize();            
        }
    }
}
