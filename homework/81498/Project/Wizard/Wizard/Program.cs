using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using Wizard.Homeworks;

namespace Wizard
{
    class Program
    {
        static void Main(string[] args)
        {
            string programAsText = "";
            string path = "E:\\IPL\\homework\\81498\\Project\\Wizard\\Wizard\\Text files\\simpleFunction.js";
            using (var reader = new StreamReader(path))
            {
                while (!reader.EndOfStream)
                {
                    programAsText += reader.ReadLine();
                    programAsText += "\n";
                }
                Console.WriteLine(programAsText);
            }

            var tokenizer = new Lexer(programAsText, path);
            var tokens = tokenizer.GenerateTokens();
            var printToPath = "E:\\IPL\\homework\\81498\\Project\\Wizard\\Wizard\\Text files\\simpleFunction.html";
            var tokenHandler = new TokenHandler(tokens, printToPath);
            tokenHandler.PrintTokens();
            tokenHandler.PrettyPrint(printToPath);

        }
    }
}
