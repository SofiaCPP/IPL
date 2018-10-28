{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TokenTypes where

import qualified Data.Text as T

data TokenKeyword
  = Break
  | Case
  | Chan
  | Const
  | Continue
  | Default
  | Defer
  | Else
  | Fallthrough
  | For
  | Func
  | Go
  | Goto
  | If
  | Import
  | Interface
  | Map
  | Package
  | Range
  | Return
  | Select
  | Struct
  | Switch
  | Type
  | Var
  deriving (Enum)

instance Show TokenKeyword where
  show Break       = "break"
  show Case        = "case"
  show Chan        = "chan"
  show Const       = "const"
  show Continue    = "continue"
  show Default     = "default"
  show Defer       = "defer"
  show Else        = "else"
  show Fallthrough = "fallthrough"
  show For         = "for"
  show Func        = "func"
  show Go          = "go"
  show Goto        = "goto"
  show If          = "if"
  show Import      = "import"
  show Interface   = "interface"
  show Map         = "map"
  show Package     = "package"
  show Range       = "range"
  show Return      = "return"
  show Select      = "select"
  show Struct      = "struct"
  show Switch      = "switch"
  show Type        = "type"
  show Var         = "var"

data TokenPredeclIdentifier
  = Boolean
  | Byte
  | Complex64
  | Complex128
  | Error
  | Float32
  | Float64
  | PlainInt
  | Int8
  | Int16
  | Int32
  | Int64
  | Rune
  | Str
  | Uint
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | UintPointer
  | BoolTrue
  | BoolFalse
  | Iota
  | Nil
  | Append
  | Cap
  | Close
  | Complex
  | Copy
  | Delete
  | Imag
  | Len
  | Make
  | New
  | Panic
  | Print
  | Println
  | Real
  | Recover
  deriving (Enum)

instance Show TokenPredeclIdentifier where
  show Boolean     = "bool"
  show Byte        = "byte"
  show Complex64   = "complex64"
  show Complex128  = "complex128"
  show Error       = "error"
  show Float32     = "float32"
  show Float64     = "float64"
  show PlainInt    = "int"
  show Int8        = "int8"
  show Int16       = "int16"
  show Int32       = "int32"
  show Int64       = "int64"
  show Rune        = "rune"
  show Str         = "string"
  show Uint        = "uint"
  show Uint8       = "uint8"
  show Uint16      = "uint16"
  show Uint32      = "uint32"
  show Uint64      = "uint64"
  show UintPointer = "uintptr"
  show BoolTrue    = "true"
  show BoolFalse   = "false"
  show Iota        = "iota"
  show Nil         = "nil"
  show Append      = "append"
  show Cap         = "cap"
  show Close       = "close"
  show Complex     = "complex"
  show Copy        = "copy"
  show Delete      = "delete"
  show Imag        = "imag"
  show Len         = "len"
  show Make        = "make"
  show New         = "new"
  show Panic       = "panic"
  show Print       = "print"
  show Println     = "println"
  show Real        = "real"
  show Recover     = "recover"

data TokenIdentifier =
  Identifier T.Text

instance Show TokenIdentifier where
  show (Identifier s) = show s

data TokenOperator
  = Plus
  | Minus
  | Mult
  | Div
  | Mod
  | BitAnd
  | BitOr
  | XOR
  | LShift
  | RShift
  | PlusEq
  | MinusEq
  | MultEq
  | DivEq
  | ModEq
  | BitAndEq
  | BitOrEq
  | XorEq
  | LShiftEq
  | MShiftEq
  | And
  | Or
  | Recv
  | PlusPlus
  | MinusMinus
  | Equal
  | LessThan
  | GreaterThan
  | Assign
  | Bang
  | Diff
  | LessThanOrEq
  | GreaterThanOrEq
  | TypeAssign
  | TripleDot
  | LParen
  | RParen
  | LSqParen
  | RSqParen
  | LBlockParen
  | RBlockParen
  | Comma
  | SemiColon
  | Colon
  | Dot
  deriving (Enum)

instance Show TokenOperator where
  show Plus            = "+"
  show Minus           = "-"
  show Mult            = "*"
  show Div             = "/"
  show Mod             = "%"
  show BitAnd          = "&"
  show BitOr           = "|"
  show XOR             = "^"
  show LShift          = "<<"
  show RShift          = ">>"
  show PlusEq          = "+="
  show MinusEq         = "-="
  show MultEq          = "*="
  show DivEq           = "/="
  show ModEq           = "%="
  show BitAndEq        = "&="
  show BitOrEq         = "|="
  show XorEq           = "^="
  show LShiftEq        = "<<="
  show MShiftEq        = ">>="
  show And             = "&&"
  show Or              = "||"
  show Recv            = "<-"
  show PlusPlus        = "++"
  show MinusMinus      = "--"
  show Equal           = "=="
  show LessThan        = "<"
  show GreaterThan     = ">"
  show Assign          = "="
  show Bang            = "!"
  show Diff            = "!="
  show LessThanOrEq    = "<="
  show GreaterThanOrEq = ">="
  show TypeAssign      = ":="
  show TripleDot       = "..."
  show LParen          = "("
  show RParen          = ")"
  show LSqParen        = "["
  show RSqParen        = "]"
  show LBlockParen     = "{"
  show RBlockParen     = "}"
  show Comma           = ","
  show SemiColon       = ";"
  show Colon           = ":"
  show Dot             = "."

data TokenWhitespace
  = Whitespace
  | NewLine
  | Tab

instance Show TokenWhitespace where
  show Whitespace = "&nbsp;"
  show NewLine    = "<br>"
  show Tab        = "&emsp;"

data TokenLiteral
  = Number T.Text
  | FloatNumber T.Text
  | OctNumber T.Text
  | HexNumber T.Text
  | RuneLiteral T.Text
  | StringLiteral T.Text

instance Show TokenLiteral where
  show (Number num)           = show num
  show (FloatNumber num)      = show num
  show (OctNumber num)        = "0" <> show num
  show (HexNumber num)        = "0x" <> show num
  show (RuneLiteral rune)     = show rune
  show (StringLiteral string) = show string

data TokenComment
  = SingleLine T.Text
  | MultiLine T.Text

instance Show TokenComment where
  show (SingleLine t) = "//" <> show t
  show (MultiLine t)  = "/*" <> show t <> "*/"

data TokenType where
  PredeclIdent :: TokenPredeclIdentifier -> TokenType
  Ident :: TokenIdentifier -> TokenType
  Keyword :: TokenKeyword -> TokenType
  Operator :: TokenOperator -> TokenType
  Literal :: TokenLiteral -> TokenType
  Space :: TokenWhitespace -> TokenType
  Comment :: TokenComment -> TokenType
  deriving (Show)
