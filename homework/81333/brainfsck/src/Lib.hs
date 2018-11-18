{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( blocks
  ) where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B

import           Data.Char
import           Text.Brainfsck.Types
import           Text.Brainfsck.Util

blocks :: Blocks -> [BFExpr] -> IO Blocks
blocks b [] = pure b
blocks (Blocks h b r) (BFRight:es) = blocks (Blocks (h + 1) b r) es
blocks (Blocks h b r) (BFLeft:es) = blocks (Blocks (max (h - 1) 0) b r) es
blocks (Blocks h b r) (BFPlus:es) = blocks (Blocks h (inc b h) r) es
blocks (Blocks h b r) (BFMinus:es) = blocks (Blocks h (dec b h) r) es
blocks (Blocks h b r) (BFPut:es) =
  blocks (Blocks h b (r <> B.singleton (chr $ b !! h))) es
blocks (Blocks h b r) (BFGet:es) = do
  v <- getChar
  blocks (Blocks h (load b h $ ord v) r) es
blocks current@(Blocks h b r) (BFLoop expr:es) = do
  if b !! h == 0
    then blocks current es
    else do
      newBlocks <- blocks current expr
      blocks newBlocks ((BFLoop expr) : es)
