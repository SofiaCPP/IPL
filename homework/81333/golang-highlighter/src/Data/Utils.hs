{-# LANGUAGE OverloadedStrings #-}

module Data.Utils where

import           Data.Char       (isAlpha, isAlphaNum)
import qualified Data.Map.Strict as MapS
import           Data.Maps       (keywords, operators, predeclIdentifiers)
import qualified Data.Text       as T
import           Data.Token
import           Data.TokenTypes
import           Debug.Trace

safeHead :: T.Text -> Maybe Char
safeHead s =
  if s == ""
    then Nothing
    else Just $ T.head s

safeTail :: T.Text -> Maybe T.Text
safeTail s =
  if s == ""
    then Nothing
    else Just $ T.tail s

token :: MapS.Map T.Text TokenType -> T.Text -> Line -> Column -> Token
token m ident line column =
  Token
    {tokenType = (MapS.!) m ident, lexeme = ident, location = (line, column)}

tokenNumber :: T.Text -> Line -> Column -> Token
tokenNumber num line column =
  Token
    { tokenType = Literal (Number num)
    , lexeme = num
    , location = (line, column + T.length num)
    }

tokenStringLiteral :: T.Text -> Line -> Column -> Token
tokenStringLiteral str line column =
  Token
    { tokenType = Literal (StringLiteral str)
    , lexeme = str
    , location = (line, column + T.length str)
    }

tokenSpace :: T.Text -> Line -> Column -> Token
tokenSpace str line column
  | str == " " =
    Token
      { tokenType = Space Whitespace
      , lexeme = "&nbsp;"
      , location = (line, column + 1)
      }
  | str == "\n" =
    Token {tokenType = Space NewLine, lexeme = "<br>", location = (line + 1, 0)}
  | str == "\t" =
    Token
      { tokenType = Space Tab
      , lexeme = T.replicate 8 "&nbsp;"
      , location = (line, column + 8)
      }

tokenSingleComment :: T.Text -> Line -> Column -> Token
tokenSingleComment str line column =
  Token
    { tokenType = Comment (SingleLine str)
    , lexeme = str
    , location = (line, column + T.length str)
    }

tokenMultiComment :: T.Text -> Line -> Column -> Token
tokenMultiComment str line column =
  Token
    { tokenType = Comment (MultiLine str)
    , lexeme = str
    , location = (line, column + T.length str)
    }

hasNewLine :: T.Text -> Bool
hasNewLine "" = False
hasNewLine s =
  if T.head s == '\n'
    then True
    else False

stripPrefix :: T.Text -> T.Text
stripPrefix = T.dropWhile (\x -> x /= ' ' && x /= '\n' && (not $ isOperator x))

getPrefix :: T.Text -> T.Text
getPrefix = T.takeWhile (\x -> x /= ' ' && x /= '\n' && (not $ isOperator x))

isOperator :: Char -> Bool
isOperator c = any (== c) (map (T.head . T.pack . show) [Plus ..])

isOnePlaceOperator :: Char -> Bool
isOnePlaceOperator c =
  c `elem`
  map (T.head . T.pack . show) (filter (\x -> (length $ show x) == 1) [Plus ..])

isWhitespace :: Char -> Bool
isWhitespace c = [c] == " "

isNewLine :: Char -> Bool
isNewLine c = [c] == "\n"

isTab :: Char -> Bool
isTab c = [c] == "\t"

isTwoPlaceOperator :: T.Text -> Bool
isTwoPlaceOperator s =
  s `elem` map (T.pack . show) (filter (\x -> (length $ show x) == 2) [Plus ..])

isThreePlaceOperator :: T.Text -> Bool
isThreePlaceOperator s =
  s `elem` map (T.pack . show) (filter (\x -> (length $ show x) == 3) [Plus ..])

getDecNumber :: T.Text -> T.Text
getDecNumber code = T.takeWhile isAlphaNum code

getStringLiteral :: T.Text -> T.Text
getStringLiteral code = "\"" <> T.takeWhile (/= '"') code <> "\""

getSingleLineComment :: T.Text -> T.Text
getSingleLineComment = T.takeWhile (/= '\n')
