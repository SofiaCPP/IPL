using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    public enum TokenType
    {
        // Single-character token types mapped to ASCII codes
        LeftParen = 40, // (
        RightParen = 41, // )
        LeftBrace = 123, // {
        RightBrace = 125, // }
        Comma = 44, // ,
        Dot = 46, // .
        Minus = 45, // -
        Plus = 43, // +
        Semicolon = 59, // ;
        Star = 42, // *
        Division = 47, // /
        Modulo = 37, // %
        BitwiseNot = 126, // ~
        BitwiseAnd = 38, // &
        BitwiseXor = 94, // ^
        BitwiseOr = 124, // |
        QuestionMark = 63, // ?
        Colon = 58, // :
        LeftSquareBracket = 91, // [
        RightSquareBracket = 93, // ]
        Bang = 33, // !
        Equal = 61, // =
        Greater = 62, // >
        Less = 60, // <
        Backslash = 92, // \

        Invalid = 127,
        NewLine,
        Tab,
        Whitespace,
        Comment,
        EOF,
        
        NotEqual, // !=
        EqualEqual, // ==
        StrictEqual, // ===
        StrictNotEqual, // !==
        GreaterEqual, // >=
        LessEqual, // <=
        MinusMinus, // --
        PlusPlus, // ++
        LeftShift, // <<
        RightShift, // >>
        LogicalAnd, // &&
        LogicalOr, // ||
        StarEqual, // *=
        DivideEqual, // /=
        ModuloEqual, // %=
        PlusEqual, // +=
        MinusEqual, // -=
        LeftShiftEqual, // <<=
        RightShiftEqual, // >>=
        BitwiseAndEqual, // &=
        BitwiseXorEqual, // ^=
        BitwiseOrEqual, // |=

        Identifier,
        String,
        Number,
        Error,

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
        New,
        Return,
        Super,
        Switch,
        This,
        Throw,
        Try,
        Typeof,
        Var,
        Let,
        Void,
        While,
        With,
        Yield,
        Null,        
        True,
        False,

        //Objects and other
        Array,
        Date,
        Eval,
        HasOwnProperty,
        Infinity,
        IsFinite,
        IsNaN,
        IsPrototypeOf,
        Length,
        Math,
        NaN,
        Name,
        NumberObj,
        Object,
        Prototype,
        StringObj,
        ToString,
        Undefined,
        ValueOf
    }
}
