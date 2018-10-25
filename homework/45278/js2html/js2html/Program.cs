using js2html.Extensions;
using System;
using System.IO;
using System.Text;

namespace js2html
{
    class Program
    {
        static void Main(string[] args)
        {
            if(args.Length == 2)
            {
                string path = args[0];
                string code = File.ReadAllText(path);
                Tokenizer tokenizer = new Tokenizer(code);
                tokenizer.Tokens = tokenizer.Tokenize();
                StringBuilder html = new StringBuilder("<!DOCTYPE html>\n<html>\n<head>\n</head>\n<body>\n");
                foreach (var str in tokenizer.ParseHtml())
                {
                    html.Append(str);
                }
                html.Append("</body>\n</html>");
                File.WriteAllText(args[1], html.ToString());
                Console.WriteLine("Finished...");
            }
        }
    }
}
