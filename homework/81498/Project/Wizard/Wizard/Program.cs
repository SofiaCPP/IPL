using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace Wizard
{
    class Program
    {
        static void Main(string[] args)
        {
            string programAsText = "";
            string path = "E:\\IPL\\homework\\81498\\Wizard\\Wizard\\Text files\\simpleFunction.js";
            using (var reader = new StreamReader(path))
            {
                while (!reader.EndOfStream)
                {
                    programAsText += reader.ReadLine();
                    programAsText += "\n";
                }
                Console.WriteLine(programAsText);

                var tokenizer = new Tokenizer(programAsText, path);
                tokenizer.PrintTokens();
            }

            using (var writer = new StreamWriter("E:\\IPL\\homework\\81498\\Wizard\\Wizard\\Text files\\simpleFunction.html"))
            {
                writer.Write("<h1> " + programAsText + "</h1>");
            }
        }
    }
}
