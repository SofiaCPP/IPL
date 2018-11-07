module Data.Token where

import qualified Data.Text       as T
import           Data.TokenTypes (TokenType)

type Lexeme = T.Text

type Line = Int

type Column = Int

data Token = Token
  { tokenType :: TokenType
  , lexeme    :: Lexeme
  , location  :: (Line, Column)
  } deriving (Show)
