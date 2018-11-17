{-# LANGUAGE OverloadedStrings #-}

module Data.Lexer
  ( tokenize
  ) where

import           Data.Char       (isAlpha, isAlphaNum)
import qualified Data.Map.Strict as MapS
import           Data.Maps       (keywords, operators, predeclIdentifiers)
import qualified Data.Text       as T
import           Data.Token
import           Data.TokenTypes
import           Data.Utils
import           Debug.Trace

parseIdent :: T.Text -> Line -> Column -> Token
parseIdent ident line column
  | MapS.member ident keywords = token keywords ident line column
  | MapS.member ident predeclIdentifiers =
    token predeclIdentifiers ident line column
  | otherwise =
    Token
      { tokenType = Ident (Identifier ident)
      , lexeme = ident
      , location = (line, column)
      }

parseSingleOperator :: Char -> Line -> Column -> Token
parseSingleOperator c line column = singleOp
  where
    singleOp = token operators (T.singleton c) line (column + 1)

parseDoubleOperator :: T.Text -> Line -> Column -> Token
parseDoubleOperator op line column = doubleOp
  where
    doubleOp = token operators op line (column + 2)

parseTripleOperator :: T.Text -> Line -> Column -> Token
parseTripleOperator op line column = tripleOp
  where
    tripleOp = token operators op line (column + 3)

tokenizeChar :: Char -> T.Text -> Line -> Column -> [Token]
tokenizeChar c code line column
  | isAlpha c || c == '_' =
    parseIdent (T.strip ident) line (column + T.length ident) :
    tokenize (stripPrefix code) line (column + T.length ident)
  | T.length code >= 2 && twoAhead == "//" =
    tokenSingleComment comment line column :
    tokenize (next comment code) line (column + T.length comment)
  | T.length code >= 3 && isThreePlaceOperator threeAhead =
    parseTripleOperator threeAhead line column :
    tokenize skipThree line (column + 3)
  | T.length code >= 2 && isTwoPlaceOperator twoAhead =
    parseDoubleOperator twoAhead line column :
    tokenize skipTwo line (column + 2)
  | isOnePlaceOperator c =
    parseSingleOperator c line column : tokenize (T.tail code) line (column + 1)
  | isTab c =
    tokenSpace "\t" line column : tokenize (T.tail code) line (column + 1)
  | isWhitespace c =
    tokenSpace " " line column : tokenize (T.tail code) line (column + 1)
  | isNewLine c =
    tokenSpace "\n" line column : tokenize (T.tail code) (line + 1) 0
  | isAlphaNum c =
    tokenNumber num line column :
    tokenize (next num code) line (column + T.length num)
  | c == '"' =
    tokenStringLiteral stringLit line column :
    tokenize (next stringLit code) line (column + T.length stringLit)
  | otherwise = tokenize (T.tail code) line column
  where
    ident = getPrefix code
    next string = T.drop (T.length string)
    twoAhead = T.pack ([c] <> [(T.head (T.tail code))])
    threeAhead = twoAhead <> T.pack ([T.head (T.tail (T.tail code))])
    comment = getSingleLineComment code
    skipTwo = T.tail $ T.tail code
    skipThree = T.tail $ T.tail $ T.tail code
    num = getDecNumber code
    stringLit = getStringLiteral (T.drop 1 code)

tokenize :: T.Text -> Line -> Column -> [Token]
-- tokenize code line column
--   | trace ("\nCode is: " <> (T.unpack code)) False = undefined
tokenize code line column =
  case safeHead code of
    Nothing -> []
    Just c  -> tokenizeChar c code line column
