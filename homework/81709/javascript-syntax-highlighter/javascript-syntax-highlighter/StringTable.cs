using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    static class StringTable
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

/*"debugger"] = TokenType::Debugger;
	m_KeyWordsTable["default"] = TokenType::Default;
	m_KeyWordsTable["delete"] = TokenType::Delete;
	m_KeyWordsTable["do"] = TokenType::Do;
	m_KeyWordsTable["else"] = TokenType::Else;
	m_KeyWordsTable["export"] = TokenType::Export;
	m_KeyWordsTable["extends"] = TokenType::Extends;
	m_KeyWordsTable["finally"] = TokenType::Finally;
	m_KeyWordsTable["for"] = TokenType::For;
	m_KeyWordsTable["function"] = TokenType::Function;
	m_KeyWordsTable["if"] = TokenType::If;
	m_KeyWordsTable["import"] = TokenType::Import;
	m_KeyWordsTable["in"] = TokenType::In;
	m_KeyWordsTable["instanceof"] = TokenType::Instanceof;
	m_KeyWordsTable["new"] = TokenType::New;
	m_KeyWordsTable["return"] = TokenType::Return;
	m_KeyWordsTable["super"] = TokenType::Super;
	m_KeyWordsTable["switch"] = TokenType::Switch;
	m_KeyWordsTable["this"] = TokenType::This;
	m_KeyWordsTable["throw"] = TokenType::Throw;
	m_KeyWordsTable["try"] = TokenType::Try;
	m_KeyWordsTable["typeof"] = TokenType::Typeof;
	m_KeyWordsTable["var"] = TokenType::Var;
	m_KeyWordsTable["let"] = TokenType::Let;
	m_KeyWordsTable["void"] = TokenType::Void;
	m_KeyWordsTable["while"] = TokenType::While;
	m_KeyWordsTable["with"] = TokenType::With;
	m_KeyWordsTable["yield"] = TokenType::Yield;
	m_KeyWordsTable["null"] = TokenType::Null;
	m_KeyWordsTable["undefined"] = TokenType::Undefined;
	m_KeyWordsTable["true"] = TokenType::True;
	m_KeyWordsTable["false"] = TokenType::False;*/
            };
        }

    }
}
