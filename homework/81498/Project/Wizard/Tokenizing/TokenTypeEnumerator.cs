using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard
{
    public enum TokenType
    {
        // Whitespaces
        Newline,
        Tab,
        Whitespace,

        // Symbols
        Colon,
        Comma,
        LeftBrace,
        LeftParenthesis,
        LeftSquareBracket,
        RightBrace,
        RightParenthesis,
        RightSquareBracket,
        Semicolon,
        SingleQuote,

        // One chararacter operators
        Bang,
        BitwiseAnd,
        BitwiseOr,
        Division,
        Dot,
        DoubleQuotes,
        Equal,
        Greater,
        Less,
        Minus,
        Modulo,
        Plus,
        QuestionMark,
        Star,

        // Two character tokens
        BangEqual,
        BitwiseAndEqual,
        BitwiseOrEqual,
        DivideEqual,
        DoubleBang,
        DoubleStar,
        EqualEqual,
        GreaterEqual,
        PlusEqual,
        PlusPlus,
        LogicalAnd,
        LogicalOr,
        LessEqual,
        StarEqual,
        MinusEqual,
        MinusMinus,
        ModuloEqual,

        // Three character tokens
        StrictEqual,
        StrictNotEqual,
        DoubleStarEqual,

        Identifier,
        Number,
        String,

        // Keywords. ECMAScript 2015
        Break,
        Case,
        Catch,
        Class,
        Const,
        Continue,
        Debugger,
        Default,
        Delete,
        Do,
        Else,
        Export,
        Extends,
        Finally,
        For,
        Function,
        If,
        Import,
        In,
        Instanceof,
        Let,
        New,
        Return,
        Super,
        Switch,
        This,
        Throw,
        Try,
        Typeof,
        Var,
        Void,
        While,
        With,
        Yield,
        Null,
        Undefined,
        True,
        False,

        Eof,
        None
    }

}
