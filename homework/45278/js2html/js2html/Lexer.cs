using System;
using System.Collections.Generic;
using System.Text;
using Sprache;

namespace js2html
{
    public static class Lexer
    {


        public static readonly string[] Keywords =
        {
            "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else",
            "export", "extends", "finally", "for", "function","if", "import", "in", "instanceof", "new", "return",
            "super", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "yield", "enum", "implements",
            "interface","let","package","private","protected","public","static","await", "abstract", "boolean","byte",
            "char","double","final","float", "goto"
        };

        public static readonly string[] Operators;

        public static string Match(string input)
        {


            return null;
        }
    }
}
