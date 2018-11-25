module Text.Brainfsck.Parser
  ( parseText
  ) where

import           Control.Applicative

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B
import           Data.Either                      (fromRight)

import           Text.Brainfsck.Types

goRight :: Parser BFExpr
goRight = do
  char '>'
  pure BFRight

goLeft :: Parser BFExpr
goLeft = do
  char '<'
  pure BFLeft

plus :: Parser BFExpr
plus = do
  char '+'
  pure BFPlus

minus :: Parser BFExpr
minus = do
  char '-'
  pure BFMinus

get :: Parser BFExpr
get = do
  char ','
  pure BFGet

put :: Parser BFExpr
put = do
  char '.'
  pure BFPut

bfsymbol :: Parser BFExpr
bfsymbol = goRight <|> goLeft <|> plus <|> minus <|> get <|> put <|> loop

loop :: Parser BFExpr
loop = do
  char '['
  m <- many' bfsymbol
  char ']'
  pure $ BFLoop m

parseText :: B.ByteString -> [BFExpr]
parseText s =
  fromRight [] $
  parseOnly (many' bfsymbol) $ B.filter ((flip elem) "+-,.<>[]") s
