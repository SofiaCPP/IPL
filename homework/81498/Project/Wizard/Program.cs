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
            string fileName = "";

            Console.WriteLine("Please Enter absolute path for the text file(sorry): \n");
            fileName = Console.ReadLine();

            Console.WriteLine("The html will be generated with name semi_pretty.html");

            using (var reader = new StreamReader(fileName))
            {
                while (!reader.EndOfStream)
                {
                    programAsText += reader.ReadLine();
                    programAsText += "\n";
                }
            }

            var tokenizer = new Lexer(programAsText, fileName);
            var tokens = tokenizer.GenerateTokens();

            var printToPath = "semi_pretty.html";
            var tokenHandler = new TokenHandler(tokens, printToPath);
            //tokenHandler.PrintTokens();
            tokenHandler.PrettyPrint(printToPath);

        }
    }
}
