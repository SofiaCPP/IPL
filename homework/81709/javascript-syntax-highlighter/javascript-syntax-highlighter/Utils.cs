using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    public static class Utils
    {
        public static bool IsWhiteSpace(char ch)
        {
            return ch == ' ';
        }

        public static bool IsCarriageReturn(char ch)
        {
            return ch == '\r';
        }

        public static bool IsNewLine(char ch)
        {
            return ch == '\n';
        }

        public static bool IsTab(char ch)
        {
            return ch == '\t';
        }

        public static bool IsStringBound(char ch)
        {
            return ch == '"' || ch == '\'';
        }
    }
}
