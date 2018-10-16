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
            string programAsText = "str = \"this is a string\"";
            int ind = programAsText.IndexOf('"', 7);
            Console.WriteLine(programAsText.Substring(7,ind - 7));
        //    string path = "E:\\IPL\\homework\\81498\\Project\\Wizard\\Wizard\\Text files\\simpleFunction.js";
        //    using (var reader = new StreamReader(path))
        //    {
        //        while (!reader.EndOfStream)
        //        {
        //            programAsText += reader.ReadLine();
        //            programAsText += "\n";
        //        }
        //        Console.WriteLine(programAsText);

        //        var tokenizer = new Tokenizer(programAsText, path);
        //        tokenizer.PrintTokens();
        //    }

        //    using (var writer = new StreamWriter("E:\\IPL\\homework\\81498\\Project\\Wizard\\Wizard\\Text files\\simpleFunction.html"))
        //    {
               
        //    }
        }
    }
}
