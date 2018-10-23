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
            string path = "E:\\IPL\\homework\\81498\\Project\\Wizard\\Wizard\\Text files\\";
            string fileName = "";
            //string currentDirectory = Directory.Move(); // finish
            //Console.WriteLine(currentDirectory);
            Console.WriteLine("Please Enter file name with js extension from the \"Text Files\" folder: \n");
            fileName = Console.ReadLine();
            path += fileName;

            using (var reader = new StreamReader(fileName))
            {
                while (!reader.EndOfStream)
                {
                    programAsText += reader.ReadLine();
                    programAsText += "\n";
                }
            }

            var tokenizer = new Lexer(programAsText, path);
            var tokens = tokenizer.GenerateTokens();

            var printToPath = "E:\\IPL\\homework\\81498\\Project\\Wizard\\Wizard\\Text files\\" +
                fileName.Remove(fileName.Length - 2, 2) + "html";

            var tokenHandler = new TokenHandler(tokens, printToPath);
            tokenHandler.PrintTokens();
            tokenHandler.PrettyPrint(printToPath);

        }
    }
}
