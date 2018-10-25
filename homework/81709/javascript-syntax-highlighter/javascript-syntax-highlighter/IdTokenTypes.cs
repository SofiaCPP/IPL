using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    static class IdTokenTypes
    {
        public static Dictionary<string, TokenType> AsMap()
        {
            return new Dictionary<string, TokenType>
            {
                {"break", TokenType.Break },
                {"case", TokenType.Case },
                {"catch", TokenType.Catch },
                {"class", TokenType.Class },
                {"const", TokenType.Const },
                {"continue", TokenType.Continue },
                {"debugger", TokenType.Debugger },
                {"default", TokenType.Default },
                {"delete", TokenType.Delete },
                {"do" , TokenType.Do },
                {"else", TokenType.Else },
                {"export", TokenType.Export },
                {"extends", TokenType.Extends },
                {"finally", TokenType.Finally },
                {"for", TokenType.For },
                {"function", TokenType.Function },
                {"if", TokenType.If },
                {"import", TokenType.Import },
                {"in", TokenType.In },
                {"instance", TokenType.Instanceof },
                {"new", TokenType.New },
                {"return", TokenType.Return },
                {"super", TokenType.Super },
                {"switch", TokenType.Switch },
                {"this", TokenType.This },
                {"throw", TokenType.Throw },
                {"try", TokenType.Try },
                {"typeof", TokenType.Typeof },
                {"var", TokenType.Var },
                {"let", TokenType.Let },
                {"void", TokenType.Void },
                {"while", TokenType.While },
                {"with", TokenType.With },
                {"yield", TokenType.Yield },
                {"null", TokenType.Null },
                {"undefine", TokenType.Undefined },
                {"true", TokenType.True },
                { "false", TokenType.False }
            };
        }

    }
}
