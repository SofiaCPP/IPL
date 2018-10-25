using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard.Misc
{
    public static class KeywordsToTokens
    {
        public static Dictionary<string, TokenType> AsMap()
        {
            return new Dictionary<string, TokenType>()
            {
                { "break", TokenType.Break },
                { "case",TokenType.Case},
                {"catch", TokenType.Catch },
                {"class", TokenType.Class },
                {"const", TokenType.Const },
                {"continue", TokenType.Continue },
                {"debugger", TokenType.Debugger },
                {"default", TokenType.Default },
                {"delete", TokenType.Delete },
                {"do", TokenType.Do },
                {"else", TokenType.Else },
                {"export", TokenType.Export },
                {"extends", TokenType.Extends },
                {"false", TokenType.False },
                {"finally", TokenType.Finally },
                {"for", TokenType.For },
                {"function", TokenType.Function },
                {"if", TokenType.If },
                {"import", TokenType.Import },
                {"in", TokenType.In },
                {"instanceof", TokenType.Instanceof },
                {"let", TokenType.Let },
                {"new", TokenType.New },
                {"return", TokenType.Return },
                {"super", TokenType.Super },
                {"switch", TokenType.Switch },
                {"this", TokenType.This },
                {"throw", TokenType.Throw },
                {"try", TokenType.Try },
                {"true", TokenType.True },
                {"typeof", TokenType.Typeof },
                {"var", TokenType.Var },
                {"void", TokenType.Void },
                {"while", TokenType.While },
                {"with", TokenType.With },
                {"yield", TokenType.Yield }
            };
        }
        public static List<TokenType> AsTokenList()
        {
            return AsMap().Values.ToList();
        }
    }
}
