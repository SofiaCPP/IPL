using JSCompiler.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JSCompiler.SyntaxHighlighter
{
    class Program
    {
        static int Main(string[] args)
        {
            if(args.Length < 2)
            {
                Console.WriteLine("Input and output destinations not specified!");
                return 1;
            }

            try
            {
                BaseInputStream stream = new BaseInputStream(args[0]);
                Lexer lex = new Lexer(stream);
                List<Token> tokens = lex.Tokenize((line, cause) => Console.Out.WriteLine($"On line {line} error: {cause}"));
                SyntaxHighlighter.GenerateHtml(tokens, args[1]);
            }
            catch(Exception e)
            {
                Console.WriteLine(e.Message);
                return 1;
            }
            
            return 0;
        }
    }
}
