{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Clex.TypesSpec
    ( spec
    ) where

import           Data.ByteString.Char8 (pack)

import           Clex.Types

import           Test.QuickCheck
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Data.ByteString.Arbitrary

spec :: Spec
spec = describe "types"
        conversionSpec

conversionSpec :: Spec
conversionSpec = describe "ByteString conversions" $ do
    prop "recognize is inverse to tokenToBS" $
        \x -> (not . isLiteral) x ==> x == recognize (tokenToBS x)
    prop "tokenToBS is inverse to recognize" $
        \x -> let x' = fromABS x in x' == tokenToBS (recognize x')

isLiteral :: Token -> Bool
isLiteral (HexNumber  _) = True
isLiteral (OctNumber  _) = True
isLiteral (Number     _) = True
isLiteral (CharLit    _) = True
isLiteral (Scientific _) = True
isLiteral (DotFloat   _) = True
isLiteral (LeadingDot _) = True
isLiteral (StringLit  _) = True
isLiteral _              = False

instance Arbitrary Token where
    arbitrary = frequency
        [ (30, nullaryGen)
        , (30, identifierGen)
        , (10, literalGen)
        ]
        where
            nullaryGen = elements
                [ Auto
                , Break
                , Case
                , Char
                , Const
                , Continue
                , Default
                , Do
                , Double
                , Else
                , Enum
                , Extern
                , Float
                , For
                , Goto
                , If
                , Int
                , Long
                , Register
                , Return
                , Short
                , Signed
                , Sizeof
                , Static
                , Struct
                , Switch
                , Typedef
                , Union
                , Unsigned
                , Void
                , Volatile
                , While
                , Ellipsis
                , RightShiftAssign
                , LeftShiftAssign
                , AddAssign
                , SubAssign
                , MulAssign
                , DivAssign
                , ModAssign
                , AndAssign
                , XorAssign
                , OrAssign
                , Incr
                , Decr
                , Ptr
                , Dot
                , And
                , Or
                , Not
                , LtEq
                , GtEq
                , Eq
                , NEq
                , Lt
                , Gt
                , Semicolon
                , LeftCurly
                , RightCurly
                , Comma
                , Colon
                , Assign
                , LeftBracket
                , RightBracket
                , LeftSquare
                , RightSquare
                , RightShift
                , LeftShift
                , BitAnd
                , BitOr
                , BitNot
                , Sub
                , Add
                , Mul
                , Div
                , Mod
                , Xor
                , Ternary
                ]
            identifierGen = Identifier <$> (pack <$> do listOf1 $ elements $ '_' : ['a'..'z'] ++ ['A'..'Z'])
                                                        `suchThat` (not . reserved)
            reserved = (`elem` allKeyWords)
            -- don't feel like overdoing this right now
            literalGen = oneof
                [ elements
                    [ HexNumber "0xb23abcf4"
                    , HexNumber "0x123ab5f4ul"
                    , HexNumber "0X123a4cf4"
                    , HexNumber "0x1e3ebcf4lull"

                    , OctNumber "0234"
                    , OctNumber "012354ul"
                    , OctNumber "012744"
                    , OctNumber "0134lull"

                    , Number "234"
                    , Number "12354ul"
                    , Number "12744"
                    , Number "134lull"
                    ]
                , elements
                    [ CharLit "'c'"
                    , CharLit "L'c'"
                    , CharLit "'\n'"
                    , CharLit "'\t'"
                    , CharLit "'a'"
                    ]
                , elements
                    [ DotFloat ".213E+10f"
                    , DotFloat ".213e-10"
                    , DotFloat ".213e-10l"
                    , DotFloat ".213e10l"
                    , DotFloat ".213l"
                    , DotFloat ".213"
                    , DotFloat ".213e9L"

                    , LeadingDot "213.2E+10f"
                    , LeadingDot "213.3e-10"
                    , LeadingDot "213.e-10l"
                    , LeadingDot "213.e10l"
                    , LeadingDot "213.e9L"
                    , LeadingDot "213.9L"
                    , LeadingDot "213.L"
                    ]
                , elements
                    [ StringLit "\"\""
                    , StringLit "L\"asdfwqe\""
                    , StringLit "\"asd\nfw\tqe\""
                    , StringLit "L\"\""
                    ]
                ]
