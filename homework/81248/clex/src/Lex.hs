{-# LANGUAGE OverloadedStrings #-}

module Lex
    ( lexC
    ) where


import           Prelude hiding (takeWhile)

import           Data.Foldable (asum)
import           Data.Word8    ( Word8, isHexDigit, isOctDigit, isAlpha
                               , _e, _E, _x, _X,_f, _F, _u, _U, _l, _L, _0
                               , _plus, _hyphen, _underscore, _quotesingle
                               , _quotedbl, _backslash, _period)

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (isDigit_w8, isSpace_w8)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Control.Applicative ((<|>))

import           Boilerplate


lexC :: ByteString -> Either String [Token]
lexC = parseOnly (cParser <* endOfInput)

cParser :: Parser [Token]
cParser = many' tokenParser

-- Order is important! For example we need to first check for a floating point number
-- with a leading digit, instead of one without, because the second one will always match the first one.
tokenParser :: Parser Token
tokenParser = asum $ concat
    [ [whitespaceParser]
    , [ scientificParser, leadingDotParser, dotFloatParser
      , numParser, hexNumParser, octNumParser, charParser, stringParser
      ]
    , map keywordParser allKeyWords
    , map operParser allOpers
    , [identifierParser]]

whitespaceParser :: Parser Token
whitespaceParser = Whitespace <$> takeWhile1 isSpace_w8

keywordParser :: ByteString -> Parser Token
keywordParser kw = do
    parsed <- recognize <$> string kw
    -- we need to check if we still have more letters after the keyword
    next <- peekWord8
    let parsed' = pure parsed
    case next of
        Nothing -> parsed'
        (Just x) -> if (not . isAlpha) x then parsed'
                                         else fail "more input after keyword"

operParser :: ByteString -> Parser Token
operParser = (recognize <$>) . string

isDigit :: Word8 -> Bool
isDigit = isDigit_w8

isLetter :: Word8 -> Bool
isLetter b = isAlpha b
          || b == _underscore

exponentParser :: Parser ByteString
exponentParser = do
    e      <- satisfy (\c -> c == _E || c == _e)
    sign   <- option "" plusMinus
    digits <- takeWhile1 isDigit_w8
    pure $ BS.singleton e <> sign <> digits
    where
        plusMinus = BS.singleton <$> satisfy (\c -> c == _plus || c == _hyphen) -- plus or minus

float :: Parser Word8
float = satisfy $
    \c -> c == _F
       || c == _f
       || c == _L
       || c == _l

unsigned :: Parser Word8
unsigned = satisfy $
    \c -> c == _U
       || c == _u
       || c == _L
       || c == _l

isxX :: Word8 -> Bool
isxX c = c == _X
      || c == _x

singleQuote :: Parser Word8
singleQuote = word8 _quotesingle

doubleQuote :: Parser Word8
doubleQuote = word8 _quotedbl

charParser :: Parser Token
charParser = do
    l   <- option "" (BS.singleton <$> word8 _L)
    _   <- singleQuote
    str <- BS.concat <$> many1' ((BS.snoc <$> string "\\" <*> anyWord8) <|> notInWeirdClass)
    sq  <- singleQuote

    pure $ CharLit ((l <> (sq `BS.cons` str)) `BS.snoc` sq)
    where
        notInWeirdClass = BS.singleton <$>
                            satisfy (\c -> c /= _backslash
                                        && c /= _quotesingle)

stringParser :: Parser Token
stringParser = do
    l   <- option "" (BS.singleton <$> word8 _L)
    _   <- doubleQuote
    str <- BS.concat <$> many' ((BS.snoc <$> string "\\" <*> anyWord8) <|> notInWeirdClass)
    dq  <- doubleQuote

    pure $ StringLit ((l <> (dq `BS.cons` str)) `BS.snoc` dq)
    where
        notInWeirdClass = BS.singleton <$>
                            satisfy (\c -> c /= _backslash
                                        && c /= _quotedbl)

zero :: Parser Word8
zero = word8 _0

hexNumParser :: Parser Token
hexNumParser = do
    z   <- zero
    x   <- satisfy isxX
    hex <- takeWhile1 isHexDigit
    f   <- option "" (BS.singleton <$> unsigned)

    pure $ HexNumber (z `BS.cons` x `BS.cons` hex <> f)

octNumParser :: Parser Token
octNumParser = do
    z   <- zero
    oct <- takeWhile1 isOctDigit
    f   <- option "" (BS.singleton <$> unsigned)

    pure $ OctNumber (z `BS.cons` oct <> f)

numParser :: Parser Token
numParser = do
    num <- takeWhile1 isDigit
    f   <- option "" (BS.singleton <$> unsigned)

    pure $ Number (num <> f)

scientificParser :: Parser Token
scientificParser = do
    digits <- takeWhile1 isDigit
    expon <- exponentParser
    f   <- option "" (BS.singleton <$> float)
    pure $ Scientific $ digits <> expon <> f

dotFloatParser :: Parser Token
dotFloatParser = do
    leadingDigits <- takeWhile isDigit
    d <- BS.singleton <$> word8 _period
    afterDigits <- takeWhile1 isDigit
    expon <- option "" exponentParser
    f   <- option "" (BS.singleton <$> float)
    pure $ DotFloat $ leadingDigits <> d <> afterDigits <> expon <> f

leadingDotParser :: Parser Token
leadingDotParser = do
    leadingDigits <- takeWhile1 isDigit
    d <- BS.singleton <$> word8 _period
    afterDigits <- takeWhile isDigit
    expon <- option "" exponentParser
    f   <- option "" (BS.singleton <$> float)
    pure $ LeadingDot $ leadingDigits <> d <> afterDigits <> expon <> f

identifierParser :: Parser Token
identifierParser = Identifier <$> takeWhile1 isLetter
