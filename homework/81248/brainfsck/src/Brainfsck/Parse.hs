module Brainfsck.Parse
    ( parse
    , ps
    ) where

import           Prelude hiding (takeWhile)

import           Data.Functor  (($>))
import           Data.Either   (fromRight)
import           Data.Foldable (asum)

import           Data.ByteString (ByteString)

import           Data.Attoparsec.ByteString.Char8 hiding (parse)

import           Brainfsck.Types
expr :: Parser Expr
expr = asum
    [ goRight
    , goLeft
    , incr
    , decr
    , out
    , inp
    , loop
    ]

eatComments :: Parser ()
eatComments = takeWhile (\x -> x `notElem` ("><+-,.[]" :: String)) $> ()

comments :: Parser a -> Parser a
comments p = eatComments *> p <* eatComments

goRight :: Parser Expr
goRight = comments $ char '>' $> GoRight

goLeft :: Parser Expr
goLeft = comments $ char '<' $> GoLeft

incr :: Parser Expr
incr = comments $ char '+' $> Incr

decr :: Parser Expr
decr = comments $ char '-' $> Decr

out :: Parser Expr
out = comments $ char '.' $> Out

inp :: Parser Expr
inp = comments $ char ',' $> In

loop :: Parser Expr
loop = comments $ Loop <$>
    (char '[' *> comments (many' expr) <* char ']')

parse :: ByteString -> Either String [Expr]
parse = parseOnly (many' expr <* endOfInput)

ps :: ByteString -> [Expr]
ps = fromRight [] . parse
