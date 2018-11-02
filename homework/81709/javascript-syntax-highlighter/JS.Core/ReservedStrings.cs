using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JSCompiler.Core
{
    public static class ReservedStrings
    {
        public static Dictionary<string, TokenType> KeywordsAsMap()
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
                {"true", TokenType.True },
                { "false", TokenType.False }
            };
        }

        public static Dictionary<string, TokenType> OthersAsMap()
        {
            return new Dictionary<string, TokenType>
            {
                {"Array", TokenType.Array },
                {"Date", TokenType.Date },
                {"eval", TokenType.Eval },
                {"hasOwnProperty", TokenType.HasOwnProperty },
                {"Infinity", TokenType.Infinity },
                {"isFinite", TokenType.IsFinite },
                {"isNaN", TokenType.IsNaN },
                {"isPrototypeOf", TokenType.IsPrototypeOf },
                {"length", TokenType.Length },
                {"Math" , TokenType.Math },
                {"NaN", TokenType.NaN },
                {"name", TokenType.Name },
                {"String", TokenType.StringObj },
                {"Number", TokenType.NumberObj },
                {"toString", TokenType.ToString },
                {"Object", TokenType.Object },
                {"undefined", TokenType.Undefined },
                {"prototype", TokenType.Prototype },
                {"valueOf", TokenType.ValueOf }
            };
        }

    }
}
