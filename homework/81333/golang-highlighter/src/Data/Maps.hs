module Data.Maps where

import qualified Data.Map.Strict as MapS
import qualified Data.Text       as T

import           Data.TokenTypes

keywords :: MapS.Map T.Text TokenType
keywords =
  let keywords = [Break ..]
   in MapS.fromList $ zip (map (T.pack . show) keywords) (map Keyword keywords)

predeclIdentifiers :: MapS.Map T.Text TokenType
predeclIdentifiers =
  let idents = [Boolean ..]
   in MapS.fromList $ zip (map (T.pack . show) idents) (map PredeclIdent idents)

operators :: MapS.Map T.Text TokenType
operators =
  let ops = [Plus ..]
   in MapS.fromList $ zip (map (T.pack . show) ops) (map Operator ops)
