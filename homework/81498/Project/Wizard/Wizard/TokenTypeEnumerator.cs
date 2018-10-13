using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard
{
    public enum TokenType
    {
        Function,
        Identifier,
        Number,
        String,

        LeftParenthesis,
        RightParenthesis,
        Comma,
        LeftBrace,
        RightBrace,
        Return,
        Semicolon,
        Plus,
        EOF,
        Whitespace,
        Tab,
        Newline,

    }

}
