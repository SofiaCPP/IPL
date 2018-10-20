{-# LANGUAGE OverloadedStrings #-}

module Clex.Types
    ( Token(..)
    , allKeyWords
    , allOpers
    , recognize
    , tokenToBS
    , tokensToBS
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

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
    | Xor
    | Ternary
    | HexNumber  ByteString
    | OctNumber  ByteString
    | Number     ByteString
    | CharLit    ByteString
    | Scientific ByteString
    | DotFloat   ByteString
    | LeadingDot ByteString
    | StringLit  ByteString
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
recognize "..." = Ellipsis
recognize ">>=" = RightShiftAssign
recognize "<<=" = LeftShiftAssign
recognize "+="  = AddAssign
recognize "-="  = SubAssign
recognize "*="  = MulAssign
recognize "/="  = DivAssign
recognize "%="  = ModAssign
recognize "&="  = AndAssign
recognize "^="  = XorAssign
recognize "|="  = OrAssign
recognize "++"  = Incr
recognize "--"  = Decr
recognize "->"  = Ptr
recognize "&&"  = And
recognize "||"  = Or
recognize "<="  = LtEq
recognize ">="  = GtEq
recognize "=="  = Eq
recognize "!="  = NEq
recognize ">>"  = RightShift
recognize "<<"  = LeftShift

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
recognize "^"  = Xor
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
    [ "..."
    , ">>="
    , "<<="
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

tokensToBS :: [Token] -> ByteString
tokensToBS = BS.concat . map tokenToBS

tokenToBS :: Token -> ByteString
tokenToBS (Whitespace bs)  = bs
tokenToBS Auto             = "auto"
tokenToBS Break            = "break"
tokenToBS Case             = "case"
tokenToBS Char             = "char"
tokenToBS Const            = "const"
tokenToBS Continue         = "continue"
tokenToBS Default          = "default"
tokenToBS Do               = "do"
tokenToBS Double           = "double"
tokenToBS Else             = "else"
tokenToBS Enum             = "enum"
tokenToBS Extern           = "extern"
tokenToBS Float            = "float"
tokenToBS For              = "for"
tokenToBS Goto             = "goto"
tokenToBS If               = "if"
tokenToBS Int              = "int"
tokenToBS Long             = "long"
tokenToBS Register         = "register"
tokenToBS Return           = "return"
tokenToBS Short            = "short"
tokenToBS Signed           = "signed"
tokenToBS Sizeof           = "sizeof"
tokenToBS Static           = "static"
tokenToBS Struct           = "struct"
tokenToBS Switch           = "switch"
tokenToBS Typedef          = "typedef"
tokenToBS Union            = "union"
tokenToBS Unsigned         = "unsigned"
tokenToBS Void             = "void"
tokenToBS Volatile         = "volatile"
tokenToBS While            = "while"
tokenToBS Ellipsis         = "..."
tokenToBS RightShiftAssign = ">>="
tokenToBS LeftShiftAssign  = "<<="
tokenToBS AddAssign        = "+="
tokenToBS SubAssign        = "-="
tokenToBS MulAssign        = "*="
tokenToBS DivAssign        = "/="
tokenToBS ModAssign        = "%="
tokenToBS AndAssign        = "&="
tokenToBS XorAssign        = "^="
tokenToBS OrAssign         = "|="
tokenToBS Incr             = "++"
tokenToBS Decr             = "--"
tokenToBS Ptr              = "->"
tokenToBS Dot              = "."
tokenToBS And              = "&&"
tokenToBS Or               = "||"
tokenToBS Not              = "!"
tokenToBS LtEq             = "<="
tokenToBS GtEq             = ">="
tokenToBS Eq               = "=="
tokenToBS NEq              = "!="
tokenToBS Lt               = "<"
tokenToBS Gt               = ">"
tokenToBS Semicolon        = ";"
tokenToBS LeftCurly        = "{"
tokenToBS RightCurly       = "}"
tokenToBS Comma            = ","
tokenToBS Colon            = ":"
tokenToBS Assign           = "="
tokenToBS LeftBracket      = "("
tokenToBS RightBracket     = ")"
tokenToBS LeftSquare       = "["
tokenToBS RightSquare      = "]"
tokenToBS RightShift       = ">>"
tokenToBS LeftShift        = "<<"
tokenToBS BitAnd           = "&"
tokenToBS BitOr            = "|"
tokenToBS BitNot           = "~"
tokenToBS Sub              = "-"
tokenToBS Add              = "+"
tokenToBS Mul              = "*"
tokenToBS Div              = "/"
tokenToBS Mod              = "%"
tokenToBS Xor              = "^"
tokenToBS Ternary          = "?"
tokenToBS (HexNumber  bs)  = bs
tokenToBS (OctNumber  bs)  = bs
tokenToBS (Number     bs)  = bs
tokenToBS (CharLit    bs)  = bs
tokenToBS (Scientific bs)  = bs
tokenToBS (DotFloat   bs)  = bs
tokenToBS (LeadingDot bs)  = bs
tokenToBS (StringLit  bs)  = bs
tokenToBS (Identifier bs)  = bs
