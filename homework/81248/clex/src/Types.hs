{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Token(..)
    , allKeyWords
    , allOpers
    , recognize
    ) where

import           Data.ByteString (ByteString)

data Token =
      Whitespace ByteString
    | Auto
    | Break
    | Case
    | Char
    | Const
    | Continue
    | Default
    | Do
    | Double
    | Else
    | Enum
    | Extern
    | Float
    | For
    | Goto
    | If
    | Int
    | Long
    | Register
    | Return
    | Short
    | Signed
    | Sizeof
    | Static
    | Struct
    | Switch
    | Typedef
    | Union
    | Unsigned
    | Void
    | Volatile
    | While
    | Ellipsis
    | RightShiftAssign
    | LeftShiftAssign
    | AddAssign
    | SubAssign
    | MulAssign
    | DivAssign
    | ModAssign
    | AndAssign
    | XorAssign
    | OrAssign
    | Incr
    | Decr
    | Ptr
    | Dot
    | And
    | Or
    | Not
    | LtEq
    | GtEq
    | Eq
    | NEq
    | Lt
    | Gt
    | Semicolon
    | LeftCurly
    | RightCurly
    | Comma
    | Colon
    | Assign
    | LeftBracket
    | RightBracket
    | LeftSquare
    | RightSquare
    | RightShift
    | LeftShift
    | BitAnd
    | BitOr
    | BitNot
    | Sub
    | Add
    | Mul
    | Div
    | Mod
    | Pow
    | Ternary
    | HexNumber ByteString
    | OctNumber ByteString
    | Number    ByteString
    | CharLit   ByteString

    | Scientific           ByteString -- ???
    | DotFloat        ByteString -- ???
    | LeadingDot ByteString -- ???

    | StringLit ByteString
    | Identifier ByteString
    deriving (Show, Eq)

-- Mapping from ByteStrings to Tokens
recognize :: ByteString -> Token
-- keywords
recognize "auto"     = Auto
recognize "break"    = Break
recognize "case"     = Case
recognize "char"     = Char
recognize "const"    = Const
recognize "continue" = Continue
recognize "default"  = Default
recognize "do"       = Do
recognize "double"   = Double
recognize "else"     = Else
recognize "enum"     = Enum
recognize "extern"   = Extern
recognize "float"    = Float
recognize "for"      = For
recognize "goto"     = Goto
recognize "if"       = If
recognize "int"      = Int
recognize "long"     = Long
recognize "register" = Register
recognize "return"   = Return
recognize "short"    = Short
recognize "signed"   = Signed
recognize "sizeof"   = Sizeof
recognize "static"   = Static
recognize "struct"   = Struct
recognize "switch"   = Switch
recognize "typedef"  = Typedef
recognize "union"    = Union
recognize "unsigned" = Unsigned
recognize "void"     = Void
recognize "volatile" = Volatile
recognize "while"    = While

-- operators
--
-- two-byte
recognize ".." = Ellipsis
recognize "+=" = AddAssign
recognize "-=" = SubAssign
recognize "*=" = MulAssign
recognize "/=" = DivAssign
recognize "%=" = ModAssign
recognize "&=" = AndAssign
recognize "^=" = XorAssign
recognize "|=" = OrAssign
recognize "++" = Incr
recognize "--" = Decr
recognize "->" = Ptr
recognize "&&" = And
recognize "||" = Or
recognize "<=" = LtEq
recognize ">=" = GtEq
recognize "==" = Eq
recognize "!=" = NEq
recognize ">>" = RightShift
recognize "<<" = LeftShift

-- one-byte
recognize "."  = Dot
recognize "<"  = Lt
recognize ">"  = Gt
recognize "&"  = BitAnd
recognize "|"  = BitOr
recognize "~"  = BitNot
recognize "!"  = Not
recognize ";"  = Semicolon
recognize "{"  = LeftCurly
recognize "}"  = RightCurly
recognize ","  = Comma
recognize ":"  = Colon
recognize "="  = Assign
recognize "("  = LeftBracket
recognize ")"  = RightBracket
recognize "["  = LeftSquare
recognize "]"  = RightSquare
recognize "-"  = Sub
recognize "+"  = Add
recognize "*"  = Mul
recognize "/"  = Div
recognize "%"  = Mod
recognize "^"  = Pow
recognize "?"  = Ternary

-- if all else fails an identifier
recognize x = Identifier x

allKeyWords :: [ByteString]
allKeyWords =
    [ "auto"
    , "break"
    , "case"
    , "char"
    , "const"
    , "continue"
    , "default"
    , "do"
    , "double"
    , "else"
    , "enum"
    , "extern"
    , "float"
    , "for"
    , "goto"
    , "if"
    , "int"
    , "long"
    , "register"
    , "return"
    , "short"
    , "signed"
    , "sizeof"
    , "static"
    , "struct"
    , "switch"
    , "typedef"
    , "union"
    , "unsigned"
    , "void"
    , "volatile"
    , "while"
    ]

-- Opers need to be seperate because they have different rules
-- for formation. For example you can have @"()()()"@, but you can't
-- have @"constconstconst"@.
allOpers :: [ByteString]
allOpers =
    [ ".."
    , "+="
    , "-="
    , "*="
    , "/="
    , "%="
    , "&="
    , "^="
    , "|="
    , "++"
    , "--"
    , "->"
    , "&&"
    , "||"
    , "<="
    , ">="
    , "=="
    , "!="
    , ">>"
    , "<<"
    , "."
    , "<"
    , ">"
    , "&"
    , "|"
    , "~"
    , "!"
    , ";"
    , "{"
    , "}"
    , ","
    , ":"
    , "="
    , "("
    , ")"
    , "["
    , "]"
    , "-"
    , "+"
    , "*"
    , "/"
    , "%"
    , "^"
    , "?"
    ]
