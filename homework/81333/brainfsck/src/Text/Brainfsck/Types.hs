{-# LANGUAGE OverloadedStrings #-}

module Text.Brainfsck.Types where

import qualified Data.ByteString.Char8 as B

data BFExpr
  = BFRight
  | BFLeft
  | BFPlus
  | BFMinus
  | BFGet
  | BFPut
  | BFLoop [BFExpr]
  deriving (Show)

data Blocks = Blocks
  { h :: Int
  , b :: [Int]
  , r :: B.ByteString
  } deriving (Show)

newBlocks :: Blocks
newBlocks = Blocks {h = 0, b = [0,0 ..], r = ""}
